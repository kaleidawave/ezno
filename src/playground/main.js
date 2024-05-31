import { EditorView, keymap, hoverTooltip } from "@codemirror/view";
import { EditorState, Text } from "@codemirror/state";
import { linter } from "@codemirror/lint";
import { defaultKeymap, indentWithTab } from "@codemirror/commands";
import { parser as jsParser } from "@lezer/javascript";
import { tags } from "@lezer/highlight";
import { HighlightStyle, syntaxHighlighting, LanguageSupport, LRLanguage } from "@codemirror/language";
import { init as init_ezno, check_with_options, get_version } from "ezno";
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
});

const STORE = "https://kaleidawave-savednamedplaygrounds.web.val.run"


let text = "const x: 2 = 3;";

const { searchParams } = new URL(location);
const code = searchParams.get("code");
const id = searchParams.get("id");

if (code) {
  text = lz.decompressFromEncodedURIComponent(code);
  setup()
} else if (id) {
  fetch(STORE + `?id=${id}`, { method: "GET" }).then(res => res.json()).then((result) => {
    if (result.content) {
      text = result.content
      setup()
    } else if (result.error) {
      alert(`Error getting code for id '${result.error}'`)
    }
  })
} else {
  setup()
}

let currentState = null;

const ROOT_PATH = "index.tsx";

async function setup() {
  await init_ezno();

  function getLinter() {
    return linter((args) => {
      text = args.state.doc.text.join("\n");
      try {
        currentState = check_with_options(ROOT_PATH, (_) => text, { lsp_mode: true, store_type_mappings: true });

        diagnosticsEntry.innerHTML = "";
        for (const diagnostic of currentState.diagnostics) {
          const entry = document.createElement("li");
          entry.innerText = diagnostic.reason;
          diagnosticsEntry.appendChild(entry);
        }

        return currentState.diagnostics.map(({ position, reason, kind }) => ({
          from: position.start,
          to: position.end,
          message: reason,
          severity: kind.toLowerCase(),
        }));
      } catch (err) {
        alert(`Error: ${err}`)
      }
    });
  }

  function getHover() {
    const cursor = hoverTooltip((view, pos, side) => {
      if (currentState) {
        const type = currentState.get_type_at_position(ROOT_PATH, pos);
        if (typeof type !== "undefined") {
          return {
            pos,
            end: pos,
            above: true,
            create(view) {
              let dom = document.createElement("div")
              dom.textContent = type
              // TODO!
              dom.style.color = "black";
              return { dom }
            }
          }
        } else {
          return null
        }
      } else {
        return null
      }
    });

    const cursorTooltipBaseTheme = EditorView.baseTheme({
      ".cm-tooltip.cm-tooltip-cursor": {
        backgroundColor: "#66b",
        color: "black",
        border: "2px solid red",
        padding: "2px 7px",
        borderRadius: "4px",
        "& .cm-tooltip-arrow:before": {
          borderTopColor: "#66b"
        },
        "& .cm-tooltip-arrow:after": {
          borderTopColor: "transparent"
        }
      }
    })

    return [cursor, cursorTooltipBaseTheme]
  }

  let editor = new EditorView({
    state: EditorState.create({
      doc: Text.of([text]),
      extensions: [
        keymap.of([...defaultKeymap, indentWithTab]),
        EditorState.tabSize.of(4),
        new LanguageSupport(LRLanguage.define({ parser: jsParser.configure({ dialect: "ts" }) }), [getLinter()]),
        syntaxHighlighting(myHighlightStyle),
        getHover(),
        theme,
      ],
    }),
    parent: editorParent,
  });

  console.log("Editor ready")
  console.log(`Running ezno@${get_version()}`)

  shareButton.addEventListener("click", () => {
    const url = new URL(location);
    const text = editor.state.doc.toString();
    const lzCompressCode = lz.compressToEncodedURIComponent(text);
    // TODO arbitrary length
    console.debug(`lzCompressCode.length=${lzCompressCode.length}`);
    if (lzCompressCode.length < 120) {
      url.searchParams.set("code", lzCompressCode);
      history.pushState({}, "", url);
    } else {
      fetch(STORE, {
        method: "POST",
        body: JSON.stringify({ content: text })
      }).then(res => res.json()).then((result) => {
        if (result.id) {
          url.searchParams.set("id", result.id);
          history.pushState({}, "", url);
        } else if (result.error) {
          alert(`Error sharing code '${result.error}'`)
        }
      })
    }
    // TODO also copy to clipboard and popup
  })
}
