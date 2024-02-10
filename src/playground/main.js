import { EditorView, keymap } from "@codemirror/view";
import { EditorState, Text } from "@codemirror/state";
import { linter } from "@codemirror/lint";
import { defaultKeymap, indentWithTab } from "@codemirror/commands";
import { parser as jsParser } from "@lezer/javascript";
import { tags } from "@lezer/highlight";
import { HighlightStyle, syntaxHighlighting, LanguageSupport, LRLanguage } from "@codemirror/language";
import { init as init_ezno, check, get_version } from "ezno";
import lz from "lz-string";

const diagnosticsEntry = document.querySelector(".diagnostics");
const editorParent = document.querySelector("#editor");
const shareButton = document.querySelector("#share");

const myHighlightStyle = HighlightStyle.define([
  { tag: tags.keyword, color: '#C792EA' },
  { tag: tags.comment, color: '#8b949e' },
  { tag: tags.controlKeyword, color: '#ffb993' },
  { tag: tags["function"], color: '#58bce3' },
  { tag: [tags.number, tags.bool], color: '#87c3e8' },
  { tag: tags.string, color: '#cae2a7' },
]);

const theme = EditorView.theme({
  "&": {
    color: "white",
    backgroundColor: "#101010",
    padding: "20px",
    fontSize: "18px",
    borderRadius: "8px"
  },
  ".cm-content": {
    caretColor: "white"
  },
  ".cm-diagnostic": {
    color: "black"
  }
})

let text = "const x: 2 = 3;";

const code = new URL(location).searchParams.get("code");
if (code) {
  text = lz.decompressFromEncodedURIComponent(code);
}

async function setup() {
  await init_ezno();

  function getLinter() {
    return linter((args) => {
      const text = args.state.doc.text.join("\n");
      try {
        const diagnostics = check("index.ts", (_) => text);

        diagnosticsEntry.innerHTML = "";
        for (const diagnostic of diagnostics) {
          const entry = document.createElement("li");
          entry.innerText = diagnostic.reason;
          diagnosticsEntry.appendChild(entry);
        }

        return diagnostics.map(({ position, reason, kind }) => ({
          from: position.start,
          to: position.end,
          message: reason,
          severity: kind.toLowerCase(),
        }));
      } catch (err) {
        alert(err)
      }
    });
  }

  let editor = new EditorView({
    state: EditorState.create({
      doc: Text.of([text]),
      extensions: [
        keymap.of([...defaultKeymap, indentWithTab]),
        EditorState.tabSize.of(4),
        new LanguageSupport(LRLanguage.define({ parser: jsParser.configure({ dialect: "ts" }) }), [getLinter()]),
        syntaxHighlighting(myHighlightStyle),
        theme
      ],
    }),
    parent: editorParent,
  });

  console.log("Editor ready")
  console.log(`Running ezno@${get_version()}`)

  shareButton.addEventListener("click", () => {
    const url = new URL(location);
    url.searchParams.set("code", lz.compressToEncodedURIComponent(editor.state.doc.toString()));
    history.pushState({}, "", url);
    // TODO copy to clipboard and popup
  })
}

setup()
