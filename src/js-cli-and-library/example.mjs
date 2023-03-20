import { build_wasm as build } from "./ezno.js";

const content = `
export function setupCounter(element) {
    let counter = 0;
    const setCounter = count => {
        counter = count;
        element.innerHTML = \`count is \${counter}\`
    };
    element.addEventListener('click', () => setCounter(counter + 1));
    setCounter(0)
};`;
console.dir(build(content, "input.js"), { depth: 5 })