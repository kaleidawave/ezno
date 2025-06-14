Getters are setters are special values, when they are read or written to it does not follow standard behavior and instead invokes a user defined function.

> They are not related to descriptors

#### Getters

```ts
let global: number = 0;
const object = {
	// This getter has an impure side effect
	get value() {
		return ++global
	},
}

object.value satisfies string
object.value satisfies boolean
```

- Expected string, found 1
- Expected boolean, found 2

#### Getter `this`

```ts
const object = {
	x: 4,
	get value(this: { x: number }) {
		return this.x
	},
}

object.value satisfies string
```

- Expected string, found 4

#### Assigning to getter

```ts
const obj = { get prop() { return 2 } };
obj.prop = "hi";
obj.prop satisfies 2;
```

- Cannot write to property 'prop' as it is a getter

#### Setter `this`

```ts
let a = 2;
const obj = {
	x: 5,
	set value(this: { x: number }, v) {
		this.x = v;
	}
}

obj.value = "some value";
obj.x satisfies 5;
```

- Expected 5, found "some value"

#### Setter assignment type

```ts
const obj = {
	set value(v: string) { }
}

obj.value = 5;
```

> TODO could the error be better?

- Argument of type 5 is not assignable to parameter of type string (in setter)

#### Setter side effect

```ts
let a = 2;
const obj = {
	x: 5,
	set value(v) {
		a = v;
	}
}

obj.value = "some value";
a satisfies 2;
```

- Expected 2, found "some value"

#### Setter return

> Returns the RHS not the return type
> TODO warning in the setter

```ts
const result = ({ set value(a) { return { a: 3 } }}).value = 5;
result satisfies string;
```

- Expected string, found 5

#### Getters AND setter

> This involves property lookup skipping setter and getters

```ts
let global: number = 0;
const object = {
	get value() {
		return global
	},
	set value(newValue: number) {
		global = newValue;
	}
}

object.value satisfies string;
object.value = 10;
object.value satisfies 10;
global satisfies 10
```

- Expected string, found 0

#### Getters AND setter can be type via `Object.defineProperty`

> TODO parameter checking as well

```ts
function func(get: () => number) {
	const obj = {};
	Object.defineProperty(obj, "value", { get });
	obj.value satisfies string
}
```

- Expected string, found number
