Current unsupported features. Eventually moved to `specification.md`

#### Array equality

```ts
const x: Array<string> = [1, "test"]
```

- TODO

### Classes

#### Classes

```ts
class X {
	constructor(value) {
		this.value = value
	}
}

const x = new X(4)
x.value satisfies string
```

- Expected string found 4

#### Class methods

```ts
class X {
	constructor(value) {
		this.value = value
	}

	getObject(b) {
		return { a: this.value, b }
	}
}

const x = new X(4)
x.getObject(2) satisfies string
```

- Expected string found {"a": 4, "b": 2, }
