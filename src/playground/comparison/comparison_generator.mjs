import { lexer, marked } from "marked";
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { join } from "node:path";
import lz from "lz-string";
import shiki from "shiki";
import ts from "./typescript_wrapper.mjs"

const { compressToEncodedURIComponent } = lz;

const dirname = import.meta.dirname;

function getSpecificationSections() {
    const content = readFileSync(join(dirname, "../../../checker/specification/specification.md")).toString();
    const output = lexer(content);

    const defaultEntry = () => ({ title: null, files: null, expectations: null });

    const sections = [];
    let currentSection = null;
    let current = defaultEntry();
    for (const item of output) {
        if (item.type === "heading") {
            if (item.depth === 3) {
                const newSection = { heading: item.text, rows: [] };
                if (currentSection) {
                    currentSection.rows.push(current);
                    current = defaultEntry();
                    sections.push(currentSection);
                }
                currentSection = newSection;
            } else if (item.depth === 4) {
                if (current.title !== null) {
                    currentSection.rows.push(current);
                    current = defaultEntry();
                }
                current.title = item.text;
            }
        } else if (item.type === "code") {
            const chunks = [];
            let currentContent = "", currentFile = "main.ts";
            for (const line of item.text.split("\n")) {
                const marker = "// in ";
                if (line.startsWith(marker)) {
                    if (currentContent.length) {
                        chunks.push([currentFile, currentContent]);
                        currentContent = ""
                    }
                    currentFile = line.substring(marker.length)
                } else {
                    currentContent += line + "\n";
                }
            }
            if (currentContent.length) {
                chunks.push([currentFile, currentContent]);
            }
            current.files = chunks;
        } else if (item.type === "list") {
            current.expectations = item.items.map(item => item.text.replaceAll("\\", ""));
        }
    }

    currentSection.rows.push(current)
    sections.push(currentSection)

    return sections
}

async function renderDifferences(sections) {
    const highlighter = await shiki.getHighlighter({ theme: 'material-theme-darker', langs: ['ts'], });
    let acc = "";
    for (const section of sections) {
        const title = section.heading;
        const href = title.toLowerCase().replaceAll(/[ \(\)'\*]+/g, "");
        acc += `<h2 id="#${href}">${title}</h2>`;
        acc += `<ul class="comparison">`;
        for (const row of section.rows) {
            const code = row.files[0][1];
            const highlightedCode = highlighter.codeToHtml(code, { lang: 'ts' });
            const title = marked(row.title).slice(3, -5);
            const href = row.title.toLowerCase().replaceAll(/[ \(\)'\*]+/g, "");
            const codeCompressed = compressToEncodedURIComponent(code);

            const eznoPlaygroundURL = "https://kaleidawave.github.io/ezno/playground/";
            const eznoPlaygroundLink = `${eznoPlaygroundURL}?raw=${encodeURIComponent(code)}`;
            const tscPlaygroundLink = `https://www.typescriptlang.org/play?#code/${codeCompressed}`;

            acc += `<li>
                <h3 id="${href}">${title}</h3>
                <div>
                    ${highlightedCode}
                    <div>
                        <div class="ezno-diagnostics">
                            <div class="checker-name">
                                <img src="./assets/ezno.svg" alt="ezno" height="14px">
                                <a href="${eznoPlaygroundLink}">(Playground)</a>
                            </div>
                            <ul>${row.ezno.map(msg => `<li>${msg}</li>`).join("")}</ul>
                        </div>
                        <div class="tsc-diagnostics">
                            <div class="checker-name">
                                <img src="./assets/typescript.svg" alt="typescript" height="20px">
                                <a href="${tscPlaygroundLink}">(Playground)</a>
                            </div>
                            <ul>${row.tsc.map(msg => `<li>${msg}</li>`).join("")}</ul>
                        </div>
                    </div>
                </div>
            </li>`;
        }
        acc += "</ul>";
    }
    return acc
}

const specification = getSpecificationSections().map((section) => {
    // , "declare"
    section.rows = section.rows.filter((row) => !["export", "import"].some(banned => row.files[0][1].includes(banned)));
    return section;
});

// console.dir(specification, { depth: 3 })

function generateTSCDifferences(specification) {
    console.time("Getting TSC differences");
    for (const section of specification) {
        section.rows = section.rows.map((row) => {
            const tsc = ts(row.files);
            return { ...row, ezno: row.expectations, tsc }
        });
    }
    console.timeEnd("Getting TSC differences");
    return specification
}

{
    // TODO temp
    let i = 0;
    for (const item of specification) {
        for (const row of item.rows) {
            i += row.expectations.length;
        }
    }
    console.log(`Found ${specification.length} specification sections with a total ${i} expectations`)
}

// writeFileSync("./specification2.json", JSON.stringify(specification, 0, 4))

generateComparisonDocument(specification)

async function generateComparisonDocument(specification) {
    const differences = generateTSCDifferences(specification);
    const description = "WIP: comparison between Ezno's and TSC's diagnostics. This is meant to highlight differences in diagnostics emitted. <strong>Bear in mind</strong> this has bias towards what Ezno supports and does not show any of the items that don't work under ezno-checker. This covers most of what Ezno implements as of August 2024.";

    const header = `<header>
    <img src="./assets/ezno.svg" alt="" height="40px">
    <p>${description}</p>
</header>`;

    const inner = await renderDifferences(differences);

    const out = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Type checker comparison</title>
    <link rel="stylesheet" href="./assets/comparison.css">
</head>

<body>
    ${header}
    <main>
        ${inner}
    </main>
</body>`;

    const place = join(dirname, "../dist/comparison");
    // `recursive: true` prevents errors when exists
    mkdirSync(place, { recursive: true });
    mkdirSync(join(place, "assets"), { recursive: true });
    writeFileSync(join(place, "index.html"), out);
    console.log("Wrote differences out")
}
