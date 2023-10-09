import { build, check, parse_expression } from "./dist/initialised.mjs";
import assert, { deepStrictEqual, equal } from "node:assert";

function buildTest() {
	// TODO
}

buildTest()

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

checkTest()

console.log(parse_expression("x = 4 + 2"))