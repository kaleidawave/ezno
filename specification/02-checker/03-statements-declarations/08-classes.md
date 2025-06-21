- Class name
- Static
- Constructor
- static fields

#### Property keys

> Property keys are synthesised once and their effects run once (as opposed to their value)

```ts
let global: number = 0;
class X {
	[global++] = "b";
}
global satisfies 0;
(new X)[0] satisfies "a";
(new X, new X);
global satisfies string;
```

- Expected 0, found 1
- Expected "a", found "b"
- Expected string, found 1

#### Property field side effects

```ts
let global: number = 0;

class X {
	property = ++global;
}

(new X()).property satisfies string;
(new X()).property satisfies 2;
(new X()).property satisfies boolean;
```

- Expected string, found 1
- Expected boolean, found 3

#### Mix of property fields and assigned

> Property fields are assigned first

```ts
let global: number = 0;

class X {
	prop1 = ++global;

	constructor() {
		this.prop2 = ++global;
	}
}

const x = new X();
x.prop1 satisfies string;
x.prop2 satisfies boolean;
```

- Expected string, found 1
- Expected boolean, found 2

#### Class methods

```ts
class X {
	constructor(value) {
		this.value = value
	}

	getObject(this: { value: any }, b) {
		return { a: this.value, b }
	}
}

const x = new X(4)
x.getObject(2) satisfies string
```

- Expected string, found { a: 4, b: 2 }

#### Automatic class constructor

```ts
class X {
	a = 2
}

(new X).a satisfies 3
```

- Expected 3, found 2

#### Static class property

```ts
class X {
	static a = 2
}

X.a satisfies 3
```

- Expected 3, found 2

#### Use before defined

> Declared same as `let` and `const`

```ts
const x = new X;
class X { }
```

- Variable 'X' used before declaration

#### Called without new

> Declared same as `let` and `const`

```ts
class X { }
const x = X();
```

- Class constructor must be called with new

#### Class type `extends`

```ts
class BaseClass {
	b: boolean = false
}

class Class extends BaseClass {
	a: number = 2
}

new Class().b satisfies 5
```

- Expected 5, found false

#### Static blocks

```ts
class X {
	static x = 2;

	static {
		const property: 4 = ++this.x;
	}
}

X.x satisfies 3;
```

- Type 3 is not assignable to type 4

#### Hoisting of class type

```ts
function doThingWithClass(instance: Class) {
	instance.prop satisfies string;
	instance.parent_prop satisfies boolean;
	instance.method(4);
}

class BaseClass {
	parent_prop: number
}

class Class extends BaseClass {
	prop: number

	method(s: string) {}
}
```

- Expected string, found number
- Expected boolean, found number
- Argument of type 4 is not assignable to parameter of type string

#### Hoisting of class type with `extends`

```ts
function doThingWithClass(instance: Class) {
	instance.a satisfies number;
	instance.b satisfies string;
}

class BaseClass {
	b: boolean
}

class Class extends BaseClass {
	a: number
}
```

- Expected string, found boolean

#### `super` call

```ts
let b: number = 0;
class Y {
	constructor(a) {
		this.a = a;
		b++;
	}
}

class X extends Y {
	constructor(a) {
		super(a);
	}
}

const x = new X("hi");
x.a satisfies "hello";
b satisfies 1;
```

- Expected "hello", found "hi"

#### Nominal-ness

```ts
class X { a: number = 2 }
class Y { a: number = 2}

function doThingWithX(x: X) {}

doThingWithX(new X());
doThingWithX(new Y())
```

- Argument of type [Y] { a: 2 } is not assignable to parameter of type X

#### Generics to constructor

```ts
class Box<T> {
	value: T;

	constructor(value: T) {
		this.value = value;
	}
}

const myBox = new Box<number>("hi");
```

- Argument of type "hi" is not assignable to parameter of type number

#### Constructor

```ts
class X {
	constructor(value) {
		this.value = value
	}
}

const x = new X(4)
x.value satisfies string
```

- Expected string, found 4
