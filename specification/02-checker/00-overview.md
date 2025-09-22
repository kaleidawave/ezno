A compiler aware of types can perform four things based on the information

- ⛑️ Correctness: subtyping through type annotations (including inferred) and syntax usage
- ⚠️ Warnings: control flow things (using disjoint and such)
- 📘 Information: information provided in an editor (via a LSP) or generating documentation (again inside editor LSP or exterior)
- 🏇 Output: using type information to cut overhead

> This is why I do not like the term "type-checker" because check implies something more narrow that what it produces
