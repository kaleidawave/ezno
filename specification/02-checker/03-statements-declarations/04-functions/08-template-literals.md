> [Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)

#### Template literal calling error

```ts
function myTag(static_parts: Array<string>, count: number) {
}

myTag`Count is ${"not a number!!"}`;
```

- Argument of type \"not a number!!\" is not assignable to parameter of type number (in template literal)

#### (untagged) Template literal

```ts
const name = "Ben";
`Hello ${name}` satisfies "Hi Ben"
```

- Expected "Hi Ben", found "Hello Ben"

#### Tagged template literal

```ts
function myTag(static_parts: Array<string>, other: string) {
	return { static_parts, other }
}

const name = "Ben";
myTag`${name}Hello ` satisfies string;
```

- Expected string, found { static_parts: ["", "Hello "], other: "Ben" }
