> This is where the idea started

#### Enabling a new model for variable values ⭐

> #TODO Removing (as) assertions and satisfies

#### Catching the exterior assignment ⭐

problem with how subtyping works and mutability.
We can check if the property being assigned meets the constraints of the object > #TODO-link

> Catches things that TS cannot check, whilest still allowing the pattern that flow does not

#### Handling properties on new objects

We want objects to be unique for disjoint analysis. We need to append, this uses the same mechanism but flags that this event is an assignment.

```ts
function construct(value) { return { value } }
```

#### Assignment to free variable

```ts
let a: number = 0
function func() {
	a = 4;
	// Important that subsequent reads use the
	// new value, not the same free variable
	a satisfies 4;
}

func()
let b: 2 = a
```

- Type 4 is not assignable to type 2

#### Assignment to free variable from parameter

> TODO move

> from parameter = specialised value

```ts
let a: number = 0
function func(c: number) {
	a = c
}

func(4)
let b: 2 = a
```

- Type 4 is not assignable to type 2

#### Free variable property update object inside function

```ts
const obj: { a: number } = { a: 2 }
function func(value: number) {
	obj.a = value
}

obj.a satisfies 2
func(4)
obj.a satisfies 3
```

- Expected 3, found 4

### [Temporal Dead Zone](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/let#temporal_dead_zone_tdz) inside functions

#### TDZ from free variable (across function)

```ts
function getX() {
	return x
}

getX satisfies () => number;

getX();

let x: number = 5;
```

- Variable 'x' used before declaration

> Not shown in the example but thanks to [#69](https://github.com/kaleidawave/ezno/pull/69) for adding the position of the error

#### TDZ errors through nested getter

```ts
function func(obj: { prop: number }) {
	return obj.prop
}

func({ get prop() { return b } });

let b: number = 0;
```

- Variable 'b' used before declaration

### Assignment through union

#### Assignment to union

> Solves the common subtyping issue between read and write properties

```ts
let myObject: { a: number } = { a: 4 }

function readA(someObject: { a: number | string }) {
	return someObject.a;
}

function setAtoString(someObject: { a: number | string }) {
	someObject.a = "hi";
}

// Allowed
readA(myObject);
setAtoString({ a: 6 });
setAtoString(myObject);
```

- Invalid assignment through parameter

> Error message could be better. Full diagnostic contains labels with more information
> `readA` is allowed, which is disallowed in Hegel, but here is allowed to preserve TSC compatibility (and because how structural subtyping is implemented)
> Not shown in the example but thanks to [#69](https://github.com/kaleidawave/ezno/pull/69) for adding the position of the error

#### Mutation

> TODO link to array

> This is part of [assignment mismatch](https://github.com/kaleidawave/ezno/issues/18)

```ts
function fakeRead(a: Array<string | number>) {
	a.push(2)
}

const array1: Array<string> = []
fakeRead(array1)
```

- Invalid assignment through parameter

### TO SORT

#### Property assignment from conditional

```ts
function getObject(condition: boolean) {
	const mainObject = { a: 2 };
	const object = condition ? mainObject : { b: 3 };
	object.c = 4;
	mainObject.c satisfies string;
	return mainObject
}
```

- Expected string, found 4

#### Assigning to parameter observed via effect

```ts
function add_property(obj: { prop: number }) {
	obj.prop += 2;
}

const obj = { prop: 4 };
add_property(obj);
obj.prop satisfies 8;
```

- Expected 8, found 6
