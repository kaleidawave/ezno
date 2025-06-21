Extending from [./reads-and-assignments.md]

#### `delete` as an effect

```ts
function dewete(param: { prop?: string }) {
	const { prop } = param;
	delete param.prop;
	return prop
}

const obj = { prop: "hi" };
dewete(obj);

obj.prop;
```

> This error *should* be from the last statement

- No property 'prop' on {}
