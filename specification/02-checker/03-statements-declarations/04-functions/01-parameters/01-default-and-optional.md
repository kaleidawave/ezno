### Default parameter

```ts
function withDefault(x: number = 1) {
	return x
}

withDefault() satisfies 2;
withDefault(3) satisfies 3;
```

- Expected 2, found 1

### Default parameter side effect

```ts
let b: number = 0
function doThing(a: number = (b += 2)) {
	return a
}

doThing(7);
b satisfies 0;
doThing();
b satisfies 1;
```

- Expected 1, found 2

### Optional parameter type

> Effectively optional parameter with the default value being undefined

```ts
function optionally(p?: number) {
	p satisfies string;
}
```

- Expected string, found number | undefined

### Calling optional parameter type

```ts
function optionally(p?: number) {
	return p
}

// Fine
optionally() satisfies undefined;
optionally(5) satisfies 5;

optionally("hello world");
```

- Argument of type "hello world" is not assignable to parameter of type number | undefined

### Default parameter side effect on parameter

```ts
function doThing(a: number, b: number = (a += 2)) {
	return a
}

doThing(3) satisfies 2;
doThing(6, 1) satisfies 6;
```

- Expected 2, found 5
