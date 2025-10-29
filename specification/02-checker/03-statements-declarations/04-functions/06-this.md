> [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/this)

The `this` reference can mean multiple things.

### Implementation

Every function type has two references

- a function id
- an optional `TypeId` representing the value of `this`

### This in object literal

```ts
const obj = {
	a: 4,
	getA(this: { a: any }) {
		return this.a
	}
}

obj.a = 5;
obj.getA() satisfies 6;
```

- Expected 6, found 5

### This passed around

```ts
function getToUpperCase(s: string) {
	return s.toUpperCase
}

getToUpperCase("hi")() satisfies "HEY";
```

- Expected "HEY", found "HI"

### This as generic argument

```ts
function callToUpperCase(s: string) {
	return s.toUpperCase()
}

callToUpperCase("hi") satisfies "HEY";
```

- Expected "HEY", found "HI"

### String internal `this` unbinding error

> Thanks to `this` checking in #127

```ts
const { toUpperCase } = "hi";

toUpperCase();
```

- The 'this' context of the function is expected to be string, found undefined

### Calling `new` on a function

```ts
function MyClass(value) {
	this.value = value
}

new MyClass("hi").value satisfies "hello"
```

- Expected "hello", found "hi"

### `new` on function with assigned prototype

```ts
function MyClass(value) {
	this.value = value
}

MyClass.prototype.other = 2;

const object = new MyClass("hi");
object.value satisfies "hi";
object.other satisfies "hello";
```

- Expected "hello", found 2

### Checking with function prototype

```ts
function MyClass(this: { other: string }) { }

MyClass.prototype.other = 2;

const m = new MyClass();
```

- The 'this' context of the function is expected to be { other: string }, found { other: 2 }

### Class `this` unbinding

> Thanks to `this` checking added in #127

```ts
class X {
	method() {
		return this;
	}
}

const { method } = new X();
method();
```

- The 'this' context of the function is expected to be X, found undefined

## To implement

### `.call`

#TODO
