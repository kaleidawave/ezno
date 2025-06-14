#### Simple early return

```ts
declare let string: string;

function stringIsHi(s: string) {
	if (s === "hi") {
		return true
	}
	return false
}

stringIsHi(string) satisfies number;
```

- Expected number, found boolean

#### More early returns

```ts
function func(value: number) {
	if (value === 3) {
		return "is three"
	}
	console.log("hi")
	return "another"
}

func satisfies (a: number) => "is three" | "another";

function loop(value: number) {
	for (let i = 0; i < 10; i++) {
		if (value === i) {
			return "something"
		}
	}
	return "another"
}

loop satisfies (a: number) => "something" | "another";

function sometimes(a: boolean) {
	if (a) {
		return "sometimes"
	}
}

sometimes satisfies (a: boolean) => string;
```

- Expected (a: boolean) => string, found (a: boolean) => "sometimes" | undefined
