import { check, parse_expression, get_version } from "./dist/initialised.mjs";
import assert, { deepStrictEqual } from "node:assert";

function buildTest() {
	// TODO
}

buildTest()

function checkTest() {
	const example = "const x: 4 = 2;"
	const output = check("input.js", (_path) => example);
	deepStrictEqual(output.diagnostics, [
		{
			reason: "Type 2 is not assignable to type 4",
			position: { start: 13, end: 14, source: 2 },
			labels: [
				[
					"Variable declared with type 4",
					{
						end: 10,
						source: 2,
						start: 9,
					},
				],
			],
			kind: "Error"
		},
	],
	);
	console.log("WASM: check test passed")
}

checkTest()

console.log(parse_expression("x = 4 + 2"))

console.log(get_version())