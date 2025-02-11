import ts from "typescript";
import { readFileSync } from "node:fs";
import { join } from "node:path";

const lib = readFileSync(join(import.meta.dirname, "./full.lib.d.ts")).toString();

export default function getDiagnosticsForTextTSC(files, rootNames = ["main.tsx"]) {
    const filesAST = new Map(Array.from(files.entries).map(([path, content]) => [path, ts.createSourceFile(path, content, ts.ScriptTarget.Latest)]));

    filesAST.set("lib.d.ts", ts.createSourceFile("lib.d.ts", lib, ts.ScriptTarget.Latest));

    /** @type {ts.CompilerOptions} */
    const options = {
        alwaysStrict: true,
        strict: true,
        strictNullChecks: true
    };
    const host = {
        fileExists: filePath => filePath === files.some(([path, _]) => path === filePath),
        getDefaultLibFileName: () => "lib.d.ts",
        directoryExists: dirPath => dirPath === "/",
        getCurrentDirectory: () => "/",
        getDirectories: () => [],
        getCanonicalFileName: fileName => fileName,
        getNewLine: () => "\n",
        getSourceFile(want) {
            return filesAST.get(want);
        },
        readFile(want) {
            return files.get(want);
        },
        useCaseSensitiveFileNames: () => true,
        writeFile: () => { }
    };

    const program = ts.createProgram({
        options: { options },
        rootNames,
        host
    });

    function getMessageText(message) {
        if (typeof message === "undefined") {
            return "no reason"
        } else if (typeof message === "object") {
            return getMessageText(message.messageText)
        } else {
            return message
        }
    }

    return ts.getPreEmitDiagnostics(program).map(getMessageText);
}