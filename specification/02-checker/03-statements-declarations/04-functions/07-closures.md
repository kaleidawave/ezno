> #TODO link...?

#### Reading variable

```ts
function kestrel(a) {
	return function (_b) {
		return a
	}
}

kestrel(3)(2) satisfies 4
```

- Expected 4, found 3

#### Nesting

```ts
function kestrel2(a) {
	return b => c => (a * b) + c
}

kestrel2(3)(2)(1) satisfies 4
```

- Expected 4, found 7

#### Carry across objects

```ts
function magicNumber(a: number) {
	return {
		plusOne() { return a + 1 },
		doubled() { return 2 * a }
	}
}

const myNumber = magicNumber(4);

// Create a one in between to test they don't have a global state
magicNumber(8).doubled() satisfies 16;

myNumber.plusOne() satisfies 5
myNumber.doubled() satisfies 6
```

- Expected 6, found 8

#### Stateful

```ts
function myClosure(a) {
	return {
		getValue() { return a },
		setValue(b) { a = b }
	}
}

const value = myClosure(4);
value.getValue() satisfies 4;
value.setValue(10);
value.getValue() satisfies 6
```

- Expected 6, found 10

#### Class constructors

```ts
function func(a: number, b: number) {
	return class {
		value: number;

		constructor() {
			this.value = a;
		}

		plusB() {
			return this.value + b
		}
	}
}

const c1 = new (func(1, 2));
c1.plusB() satisfies 3;

const c2 = new (func(6, 8));
c2.plusB() satisfies string;
```

- Expected string, found 14

#### Getters closures

```ts
function Closure(n: string) {
	return { get value() { return n }, set value(newValue: string) { n = newValue;	} };
}

let b = Closure("hi");
b.value = "something";
b.value satisfies number;
```

- Expected number, found "something"
