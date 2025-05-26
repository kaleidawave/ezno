There are three number instrincs
- `LessThan`
- `GreaterThan`
- `MultipleOf`

They can be combined to generate `LessThanEqual` version

> These exist in the standard typing

These are inferred
```ts
if (x < 5) {
	print_type(x)
}
```
These can be passed through operators (experimental) > #TODO example with addition etc

Disjoint analysis can catch impossible conditions > #TODO less than and multiple of

There are two motivations for this
- `Array.prototype.length`
- WASM targets (JS does not distinguish, although they do exist sometimes at runtime?)

> Also because I was taking a course on numbers (that started with residues, before moving on to FLT for 8n etc) and wanted to use some things in it

#### Not not a number

> #TODO
