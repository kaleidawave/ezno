import { build } from "./dist/initialized.mjs";
import { deepStrictEqual } from "node:assert";

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

const output = build(() => content, "input.js");

deepStrictEqual(output, {
	Ok: {
		outputs: [
			{
				output_path: "out.js",
				content:
					"export function setupCounter(element){let counter=0;const setCounter=count=>{counter=count;element.innerHTML=`count is ${counter}`};element.addEventListener('click',()=>setCounter(counter+1));setCounter(0)}",
				mappings: "",
			},
		],
		temp_warnings_and_infos: [],
	},
});

console.log("WASM: build test passed")