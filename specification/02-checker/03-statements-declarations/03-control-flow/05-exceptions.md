> Also see events

#### Try-catch and throw

```ts
try {
	throw 2
} catch (err) {
	err satisfies string
}

console.log("Error caught!")
```

- Expected string, found 2

#### Throw effects carry through

```ts
function throwType(a) {
	throw a
}

try {
	throwType(3)
} catch (err) {
	err satisfies string
}

console.log("Error caught!")
```

- Expected string, found 3

#### Catch annotation

> Thanks to #131

```ts
try {
	throw 3
} catch (err: string) {
	console.log(err)
}

console.log("Error caught!")
```

- Cannot catch type string because the try block throws 3
