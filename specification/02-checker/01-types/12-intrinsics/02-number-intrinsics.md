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

### Not not a number

> #TODO

### Number intrinsics

```ts
5 satisfies MultipleOf<2>;
4 satisfies MultipleOf<2>;

6 satisfies GreaterThan<2>;
-4 satisfies GreaterThan<2>;

2 satisfies LessThan<2>;
-3 satisfies LessThan<2>;
```

- Expected MultipleOf\<2>, found 5
- Expected GreaterThan\<2>, found -4
- Expected LessThan\<2>, found 2

## Inferred ranges

### Ranges for internal types

```ts
function func(a: number) {
	(Math.sin(a) > -2) satisfies true;
	(Math.sin(a) > -1) satisfies string;
}
```

With advanced_numbers

- Expected string, found boolean

### Ranges after operations

```ts
function func(a: number) {
	(Math.sin(a) * 5) satisfies null;
	((Math.sin(a) + 10)) * 2 satisfies string;
}
```

With advanced_numbers

- Expected null, found GreaterThan<-5> & LessThan<5> | -5 | 5
- Expected string, found GreaterThan<18> & LessThan<22> | 18 | 22

### Modulo range

```ts
function func(x: number) {
  return x % 5 === 6;
}
```

With advanced_numbers

- This equality is always false as ExclusiveRange<-5, 5> and 6 have no overlap

### More

### Transistivity

```ts
function func(a: number, b: number, c: number) {
  if (a < b && b < c)  {
    const cond = (a < c) satisfies 5;
  }
}
```

With advanced_numbers

- Expected 5, found true
