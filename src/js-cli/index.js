const { build_wasm } = require("./ezno")

const content = "const x = !t ? 4 : 5;";
console.dir(build_wasm(content, "input"), { depth: 5 })
