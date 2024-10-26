import { check, parse_expression, get_version, experimental_build } from "../dist/initialised.mjs";
import { deepStrictEqual } from "node:assert";
import { test } from "node:test";
import { inspect } from "node:util";

console.log(`Running ezno@${get_version()}*`)

test("Type checking on code diagnostics", (t) => {
	t.test("type check", () => {
		const example = "const x: 4 = 2;"
		const output = check("input.ts", (_path) => example);
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
				kind: "error"
			},
		]);
	});
});

test("Compiling", (t) => {
	t.test("Compile", () => {
		const example = "const x: 4 = 2 + 2;"
		const output = experimental_build("input.ts", (_path) => example, true);
		deepStrictEqual(output, {
			Ok: {
				outputs: [{ output_path: 'out.js', content: 'const x=2+2', mappings: '' }],
				diagnostics: []
			}
		});
		// console.log(inspect(output, { depth: Infinity, colors: true }));
	})
});

test("Parsing", (t) => {
	t.test("expressions", () => {
		const expression = parse_expression("x = 4 + 2");

		// console.log(inspect(expression, { depth: Infinity, colors: true }));

		deepStrictEqual(expression, {
			Ok: {
				Assignment: {
					lhs: {
						VariableOrPropertyAccess: { Variable: ['x', { start: 0, end: 1 }] }
					},
					rhs: {
						BinaryOperation: {
							lhs: { NumberLiteral: [{ Number: 4 }, { start: 4, end: 5 }] },
							operator: 'Add',
							rhs: { NumberLiteral: [{ Number: 2 }, { start: 8, end: 9 }] },
							position: { start: 4, end: 9 }
						}
					},
					position: { start: 0, end: 9 }
				}
			}
		});
	})
})
