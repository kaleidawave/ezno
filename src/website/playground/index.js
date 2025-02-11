import { EditorView, keymap, hoverTooltip, lineNumbers } from "@codemirror/view";
import { EditorState, Text, EditorSelection } from "@codemirror/state";
import { linter } from "@codemirror/lint";
import { defaultKeymap, indentWithTab, toggleLineComment } from "@codemirror/commands";
import { parser as jsParser } from "@lezer/javascript";
import { tags } from "@lezer/highlight";
import { HighlightStyle, syntaxHighlighting, LanguageSupport, LRLanguage } from "@codemirror/language";
import { init as init_ezno, check, get_version, experimental_build } from "ezno";

const body = document.body;
const diagnosticsEntry = body.querySelector(".diagnostics"),
  editorParent = body.querySelector("#editor"),
  shareButton = body.querySelector("#share"),
  buildButton = body.querySelector("#build"),
  timeOutput = body.querySelector("#time");

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
    borderRadius: "8px",
    fontSize: "14px",
    fontFamily: "'JetBrains Mono', 'Fira Code', Consolas, 'Courier New', monospace"
  },
  ".cm-scroller": {
    fontFamily: "'JetBrains Mono', 'Fira Code', Consolas, 'Courier New', monospace"
  },
  ".cm-content": {
    caretColor: "white"
  },
  ".cm-diagnostic-error": {
    borderLeft: "5px solid #52050b",
    padding: "4px 6px",
  },
  ".cm-tooltip-hover": {
    color: "#bebebe",
    backgroundColor: "black",
    borderRadius: "4px",
  },
  ".cm-diagnosticSource": {
    display: "inline",
    position: "relative",
    top: "-2px",
    marginLeft: "10px",
  },
  ".cm-tooltip-section:not(.cm-tooltip-lint)": {
    padding: "4px 6px"
  },
  ".cm-gutters": {
    background: "none",
    paddingRight: "10px"
  }
}, { dark: true });

const STORE = "https://kaleidawave-savednamedplaygrounds.web.val.run"

const defaultCode = "const x: string = 5;"
let text = defaultCode;

const { searchParams } = new URL(location);
const id = searchParams.get("id");
const raw = searchParams.get("raw");
const LOCAL_STORAGE_KEY = "ezno:playground_code";

if (id) {
  fetch(STORE + `?id=${id}`, { method: "GET" }).then(res => res.json()).then((result) => {
    if (result.content) {
      text = result.content
      setup()
    } else if (result.error) {
      alert(`Error getting code for id '${result.error}'`)
    }
  })
} else {
  if (raw) {
    text = raw;
  } else {
    text = window.localStorage.getItem(LOCAL_STORAGE_KEY) ?? defaultCode;
  }
  setup()
}

/** Used for not resharing same code, Mutable as set after sharing */
let initialText = text;

let currentState = null;

const ROOT_PATH = "index.tsx";
const options = {
  // Allow partial syntax
  lsp_mode: true,
  // For hover
  store_type_mappings: true,
  // For showing off
  advanced_numbers: true
};

async function setup() {
  await init_ezno();

  const checkerNameAndVersion = `ezno@${get_version()}`;

  function runChecker() {
    return linter((args) => {
      text = args.state.doc.text.join("\n");
      localStorage.setItem(LOCAL_STORAGE_KEY, text);
      try {
        const start = performance.now();
        currentState = check(ROOT_PATH, (_) => text, options);
        const elapsed = performance.now() - start;
        timeOutput.innerText = `Parsed & checked in ${Math.trunc(elapsed)}ms`;

        diagnosticsEntry.innerHTML = "";
        for (const diagnostic of currentState.diagnostics) {
          const entry = document.createElement("li");
          entry.setAttribute("data-range", `${diagnostic.position.start}-${diagnostic.position.end}`)
          entry.innerText = diagnostic.reason;
          diagnosticsEntry.appendChild(entry);
        }

        return currentState.diagnostics.map(({ position, reason, kind }) => ({
          from: position.start,
          to: position.end,
          message: reason,
          severity: kind.toLowerCase(),
          source: checkerNameAndVersion
          // renderMessage(_view) {
          //   console.log("here");
          //   const dom = document.createElement("div")
          //   const s1 = document.createElement("span");
          //   s1.textContent = reason;
          //   dom.append(s1);
          //   const s2 = document.createElement("span");
          //   s2.classList.add("from");
          //   s2.textContent = reason;
          //   dom.append(s2);
          //   // setTimeout(() => { debugger; }, 2000);
          //   return { dom }
          // }
        }));
      } catch (err) {
        alert(`Error: ${err}`)
      }
    });
  }

  function getHover() {
    function showHover(_view, pos, _side) {
      if (currentState) {
        // TODO with position information so highlights item
        const type = currentState.get_type_at_position(ROOT_PATH, pos);
        if (typeof type !== "undefined") {
          return {
            pos,
            end: pos,
            above: true,
            create(_view) {
              const dom = document.createElement("span")
              dom.textContent = type;
              return { dom }
            },
          }
        } else {
          return null
        }
      } else {
        return null
      }
    }

    return hoverTooltip(showHover)
  }

  const tsLanguage = LRLanguage.define({
    parser: jsParser.configure({ dialect: "ts" }),
    languageData: { commentTokens: { line: "// " } }
  });

  const editor = new EditorView({
    state: EditorState.create({
      doc: Text.of(text.split("\n")),
      extensions: [
        lineNumbers(),
        keymap.of([...defaultKeymap, indentWithTab, toggleLineComment]),
        EditorState.tabSize.of(4),
        new LanguageSupport(tsLanguage, [runChecker()]),
        syntaxHighlighting(myHighlightStyle),
        getHover(),
        theme,
      ],
    }),
    parent: editorParent,
  });

  console.debug(`Editor ready. Running ${checkerNameAndVersion}`);
  document.querySelector("#version").innerText = `Running ${checkerNameAndVersion}`;

  document.querySelector(".diagnostics").addEventListener("click", (ev) => {
    const li = ev.target;
    const [start, end] = li.getAttribute("data-range").split("-");
    // TODO doesn't do what expected to do
    const _response = editor.dispatch({ selection: EditorSelection.range(start, end), scrollIntoView: true });
  });

  shareButton.addEventListener("click", () => {
    const url = new URL(location);

    if (text !== initialText) {
      // This + `if` statement prevents spam
      initialText = text;

      fetch(STORE, {
        method: "POST",
        body: JSON.stringify({ content: text })
      })
        .then(res => res.json())
        .then((result) => {
          if (result.id) {
            url.searchParams.set("id", result.id);
            url.searchParams.delete("raw");
            history.pushState({}, "", url);
            navigator.clipboard
              .writeText(url.toString())
              .then(() => {
                alert("Share URL copied to clipboard")
              });
          } else if (result.error) {
            alert(`Error sharing code '${result.error}'`)
          }
        })
    } else {
      alert("Sharing existing code")
    }
  });

  buildButton.addEventListener("click", () => {
    const strip_whitespace = body.querySelector("#strip_whitespace").checked;
    const tree_shake = body.querySelector("#tree_shake").checked;
    const results = experimental_build("index.tsx", (_) => text, { strip_whitespace, tree_shake });
    let s = "// Build output:\n";
    for (const artifact of results.artifacts) {
      s += `// ${artifact.output_path}\n${artifact.content}`;
    }
    body.querySelector("#build-output").innerHTML = s;
  });
}
