import { extract_code_blocks, initSync } from "@bengineering/simple-markdown-parser/uninitialised";
import { readFileSync } from "node:fs";
import { compress as compressToEncodedURIComponent } from '@amoutonbrady/lz-string'
import shiki from "shiki";
import check_with_tsc from "./tsc_api.mjs";

initSync({ module: readFileSync("./node_modules/@bengineering/simple-markdown-parser/build_bg.wasm") });

function escapeHTML(code) {
    const lookup = {
        '&': "&amp;",
        '"': "&quot;",
        '\'': "&apos;",
        '<': "&lt;",
        '>': "&gt;"
    };
    return code.replace(/[&"'<>]/g, c => lookup[c]);
}

async function renderDifferences(sections) {
    const highlighter = await shiki.getHighlighter({ theme: 'material-theme-darker', langs: ['ts'], });
    let acc = "";
    for (const section in sections) {
        const title = section;
        const href = title.toLowerCase().replaceAll(/[ \(\)'\*]+/g, "");
        acc += `<h2 id="#${href}">${title}</h2>`;
        acc += `<ul class="comparison">`;

        for (const row of sections[section]) {
            const code = row.files.get("main.tsx");
            const highlightedCode = highlighter.codeToHtml(code, { lang: 'ts' });
            const title = row.location.at(-1)
            if (!title) {
                console.log("no location?", row);
                continue;
            }

            // TODO run through markdown render
            const titleHTML = title;
            const href = escapeHTML(title.toLowerCase().replaceAll(/[ \(\)'\*]+/g, ""));
            const codeCompressed = compressToEncodedURIComponent(code);

            const eznoPlaygroundURL = "https://kaleidawave.github.io/ezno/playground/";
            const eznoPlaygroundLink = `${eznoPlaygroundURL}?raw=${encodeURIComponent(code)}`;
            const tscPlaygroundLink = `https://www.typescriptlang.org/play?#code/${codeCompressed}`;

            acc += `<li>
                <h3 id="${href}">${titleHTML}</h3>
                <div>
                    ${highlightedCode}
                    <div>
                        <div class="ezno-diagnostics">
                            <div class="checker-name">
                                <img src="./assets/ezno.svg" alt="ezno" height="14px">
                                <a href="${eznoPlaygroundLink}">(Playground)</a>
                            </div>
                            <ul>${row.ezno.map(msg => `<li>${escapeHTML(msg)}</li>`).join("")}</ul>
                        </div>
                        <div class="tsc-diagnostics">
                            <div class="checker-name">
                                <img src="./assets/typescript.svg" alt="typescript" height="20px">
                                <a href="${tscPlaygroundLink}">(Playground)</a>
                            </div>
                            <ul>${row.tsc.map(msg => `<li>${escapeHTML(msg)}</li>`).join("")}</ul>
                        </div>
                    </div>
                </div>
            </li>`;
        }

        acc += "</ul>";
    }
    return acc
}

function codeToFiles(code) {
    const files = new Map;
    let acc = "";
    let path = "main.tsx";
    for (const line of code.split("\n")) {
        if (line.startsWith("// in ")) {
            if (acc.length) files.set(path, acc);
            path = line.slice("// in ".length);
        } else {
            acc += line;
        }
    }
    if (acc.length) files.set(path, acc);
    return files
}

export async function generate(content) {
    console.time("Extract code blocks");
    const specification = extract_code_blocks(content);

    const parts = specification
        .filter((part) => !["export", "import"].some(banned => part.code.includes(banned)))
        .map(part => ({ ...part, files: codeToFiles(part.code) }));

    // TODO temp
    {
        let i = 0;
        for (const part of parts) {
            i += part.items.length;
        }
        console.log(`Found ${parts.length} specification sections with a total ${i} expectations`)
    }
    console.timeEnd("Extract code blocks");

    const sections = Object.groupBy(parts, (part) => part.location[2]);

    return generateComparisonDocument(sections)
}


function generateTSCDifferences(specification) {
    console.time("Getting TSC differences");
    const out = {};
    for (const section in specification) {
        const parts = [];
        for (const item of specification[section]) {
            // const tsc_diagnostics = check_with_tsc(item.files);
            const tsc_diagnostics = [];
            parts.push({ ...item, ezno: item.items, tsc: tsc_diagnostics });
        }
        out[section] = parts;
    }
    console.timeEnd("Getting TSC differences");
    return out
}

async function generateComparisonDocument(specification) {
    const differences = generateTSCDifferences(specification);

    const description = "WIP: comparison between Ezno's and TSC's diagnostics. This is meant to highlight differences in diagnostics emitted. <strong>Bear in mind</strong> this has bias towards what Ezno supports and does not show any of the items that don't work under ezno-checker. This covers most of what Ezno implements as of December 2024.";

    const header = `<header>
    <img src="../assets/ezno.svg" alt="EZNO" height="40px">
    <p>${description}</p>
</header>`;

    const inner = await renderDifferences(differences);

    const out = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Type checker comparison</title>
    <meta property="twitter:image" content="../assets/banner.png">
    <meta property="og:image" content="../assets/banner.png">

    <link rel="stylesheet" href="../assets/index.css">
    <link rel="icon" href="../assets/ezno.svg">
</head>

<body>
    ${header}
    <main>
        ${inner}
    </main>
</body>`;
    return out
}
