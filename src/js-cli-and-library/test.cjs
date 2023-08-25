const { build, check } = require("./dist/initialized.cjs");
const { deepStrictEqual } = require("node:assert");

function buildTest() {
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

	const output = build((_path) => content, "input.js");

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
			temp_diagnostics: [],
		},
	});

	console.log("WASM: build test passed")
}

function checkTest() {
	const example = "const x: 4 = 2;"
	const output = check((_path) => example, "input.js");
	deepStrictEqual(output, [
		{
			type: "PositionWithAdditionLabels",
			reason: "Type 2 is not assignable to type 4",
			position: { start: 13, end: 14, source: 1 },
			labels: [
				[
					"Variable declared with type 4",
					{
						end: 10,
						source: 1,
						start: 9,
					},
				],
			],
			kind: { type: "Error" }
		},
	],
	);
	console.log("WASM: check test passed")
}

buildTest()
checkTest()