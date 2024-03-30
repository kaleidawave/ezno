() => {
	const x: number = 2
	const y: string = 2
	const z: object = 4

	const a = 3
	const b: string = a

	doesNotExist = 4;

	const my_obj = { a: 2 }
	const three: 3 = my_obj.a

	let global = 0;
	const object = {
		// This getter has an impure side effect
		get getValue() {
			return ++global
		},
	}
	
	object.getValue satisfies string
	object.getValue satisfies boolean

	const obj1 = { a: 2, b: 3 };
	const obj2 = { b: 4, ...obj1, a: 6 };
	
	obj2.b satisfies 100;
	obj2.a satisfies boolean;

	const obj = { a: 2 }
	
	function setProperty(key: string, value) {
		obj[key] = value;
	}
	
	setProperty("b", 6)
	obj satisfies string;

	(4 === 2) satisfies true;
	(4 !== 2) satisfies string;

	(Math.PI > 3) satisfies true;
	(4 < 2) satisfies true;
	(4 > 2) satisfies number;
	(6 >= 2) satisfies string;
	(6 <= 2) satisfies 5;

	"hi".toUpperCase() satisfies number

	Math.cos(0) satisfies 0
	Math.sqrt(16) satisfies 1
	Math.floor(723.22) satisfies 2;

	("something"[2]) satisfies number;

	function func(a: number) {
		a satisfies string
	}

	function add_property(obj: { prop: number }) {
		obj.prop = 2;
		(obj.prop satisfies 4);
	}

	function myThrow() {
		throw "err!"
	}
	
	myThrow satisfies string;

	function getSecond1<T, U>(p1: T, p2: U): U {
		return p1
	}
	
	function getSecond2<T, U>(p1: T, p2: U): U {
		return p2
	}

	function setFirst1<T, U>(a: T, b: U) {
		const a2: T = a;
	}
	
	function setFirst2<T, U>(a: T, b: U) {
		const a2: U = a;
	}

	function createObject1<T, U>(a: T, b: U): { a: T, b: U } {
		return { a, b }
	}
	
	function createObject2<T, U>(a: T, b: U): { a: U, b: U } {
		return { a, b }
	}

	function map(a: (a: number) => number) {}
	
	// No annotation on `a`. But error comes from body
	// (rather than parameter assignment)
	map(a => a.t)

	function alterParameter(a: number, b: { prop: string }) {
		a = 2;
		a = "hi";
	
		b.prop = 3;
	
		b.prop = "hello";
		// Observed. TODO disabled because of possible impure (getters etc)
		// b.prop satisfies "hello";
	}

	function myRestFunction(...r: string[]) {
		r satisfies boolean;
	}

	function myFunction({ a }: { a: number }) {
		a satisfies boolean;
		return a
	}
	
	myFunction({ a: 6 }) satisfies string;

	function getNumber1(): number {
	    return 4
	}
	
	function getNumber2() {
	    return 6
	}
	
	getNumber1 satisfies () => 4;
	getNumber2 satisfies () => 6;
	
	getNumber1() satisfies 4;
	getNumber2() satisfies 6;

	function id(a) {
		return a
	}
	
	const d: 3 = id(2)

	function addTwoToResult(func: (n: number) => number) {
		return func(4) + 2
	}
	
	addTwoToResult((a: number) => a * 4) satisfies 5

	function call(func: (n: number) => number) {
		return func(9)
	}
	
	call(Math.sqrt) satisfies 2

	function floorPlusB(a: number, b: number) {
		return Math.floor(a) + b
	}
	
	floorPlusB(100.22, 5) satisfies 8

	function getToUpperCase(s: string) {
		return s.toUpperCase
	}
	
	getToUpperCase("hi")() satisfies "HEY";

	function callToUpperCase(s: string) {
		return s.toUpperCase()
	}
	
	callToUpperCase("hi") satisfies "HEY";

	function MyClass(value) {
		this.value = value
	}
	
	new MyClass("hi").value satisfies "hello"

	function withDefault(x: number = 1) {
		return x
	}
	
	withDefault() satisfies 2;
	withDefault(3) satisfies 3;

	function myTag(static_parts: Array<string>, name: string) {
		return static_parts[0] + name
	}
	
	const name = "Ben";
	myTag`${name}Hello ` satisfies "Hi Ben"

	function doThing(a, b = (a += 2)) {
		return a
	}
	
	doThing(3) satisfies 2;
	doThing(6, 1) satisfies 6;

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

	function getObject(condition: boolean) {
		const mainObject = { a: 2 };
		const object = condition ? mainObject : { b: 3 };
		object.c = 4;
		mainObject.c satisfies string;
		return mainObject
	}

	function kestrel(a) {
		return function (_b) {
			return a
		}
	}
	
	kestrel(3)(2) satisfies 4

	function kestrel2(a) {
		return _b => _c => a
	}
	
	kestrel2(3)(2)(6) satisfies 4

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

	function isNegative(x: number) {
		return x < 0 ? "negative" : "positive"
	}
	isNegative(-4) satisfies number
	isNegative(4) satisfies boolean

	function print_number(value: number) {
		if (value === 0) {
			return "zero"
		} else if (value === 1) {
			return "one"
		} else {
			return "some number"
		}
	}
	
	print_number(0) satisfies "zero"
	print_number(0) satisfies "some number"
	print_number(1) satisfies "ONE"
	print_number(100) satisfies "100"
	print_number(-1) satisfies "TWO"

	function loop(n: number, c: string) {
		let a: string = c;
		let i: number = 0;
		while (i++ < n) {
			a += c
		}
		return a
	}
	
	loop(10, "!") satisfies number;

	let properties: string = "";
	for (const property in { a: 1, b: 2, c: 3 }) {
		properties += property;
	}
	properties satisfies boolean;

	const myArray = [6, "hi"]
	myArray.pop() satisfies 3;
	myArray.length satisfies 1;

	[6, 8, 10].map(x => x + 1) satisfies [7, 8, 11];
	
	[1, 2, 3].filter(x => x % 2 === 0) satisfies [2];

	[1, 2, 3].find(x => x % 2 === 0) satisfies 4;
	
	// [1, 2, 3].includes(6) satisfies string;

	const regexp = /hi/ satisfies string;

	undefined satisfies null;
	null satisfies undefined;

	(void 2) satisfies string;

	interface X {
		a: string,
		b: boolean
	}
	
	{
		interface X {
			c: number
		}
		
		const x: X = { a: "field", b: false, c: false }
		const y: X = { a: "field", b: false, c: 2 }
	}

	let first = second;
	
	let second = 2;

	s satisfies string;
	var s = "hello"
	s satisfies number;

	try {
		throw 2
	} catch (err) {
		err satisfies string
	}

	function getProp(obj: { prop: 3 } | { prop: 2 }) {
		return obj.prop
	}
	
	getProp satisfies string

	function getA<T extends { a: string }>(p: T) {
		return p.a
	}
	
	getA({ p: 2 })

	function runWithCallback(cb: () => void): void {
		cb() satisfies string;
	
		return 5;
	}
	
	runWithCallback(() => 3)

	interface ThePrimitives {
		a: number,
		b: string,
		c: boolean
	}
	
	2 satisfies ThePrimitives["b"];

	function getFirst(array: number[]) {
		return array[0]
	}
	
	getFirst satisfies boolean;

	function getSecondCharacter(s: string) {
		return s[1]
	}
	
	getSecondCharacter satisfies boolean;
	getSecondCharacter("string") satisfies "b";

	function or1<T, U>(obj: T | U): U | T { return obj }
	
	function or2(obj: string | number): number | string { return obj }
	
	// Lack of symmetry
	function or3(obj: string | number): number { return obj }

	function and1<T, U>(obj: T & U): U & T { return obj }
	
	// Lack of symmetry
	function and2<T, U>(obj: T): U & T { return obj }

	function distribute1<T, U, V>(obj: (T | U) & V): (T & V) | (U & V) { return obj }
	
	function distribute2<T, U, V>(obj: V & (T | U)): (T & V) | (U & V) { return obj }
	
	// bad!
	function distribute3<T, U, V>(obj: (T | U) & V): (T & U) | (U & V) { return obj }

	function get(obj: {a: 2} | { b: 3 }) {
		return obj.a
	}

	interface Optional {
	    a?: "hi"
	}
	
	const op1: Optional = {}
	const op2: Optional = { a: "hello" }

	interface Wrapper<T> {
		internal: T
	}
	
	const my_wrapped: Wrapper<number> = { internal: "hi" }

	const numbers1: Array<number> = [1, 2, "3"]
	const numbers2: Array<string> = ["hi", "3"]

	let c: Array<number> = []
	
	function add() {
		c.push("hi")
	}

	function callFunction<T>(fn: (p: T) => void) {
	    // ...
	}
	
	callFunction<string>(a => {
	    a satisfies number;
	})

	{
	    const obj = { a: true, b: false };
	    const x: { a: boolean } = obj, y: { b: boolean } = obj;
	
	    obj.a = "yo";
	    obj.b = "wassup";
	}
	
	{
	    // and in the same assignment through a cycle
	    const obj = { a: 2, b: 3 }; obj.c = obj;
	    const something: { a: number, c: { b: number } } = obj;
	
	    obj.a = "hi";
	    obj.b = "hello";
	}
};

() => {
	let x: number = 3
	x = "hello world"

	let a = 2
	a = "hello world"
	let b: boolean = a

	const my_obj: { a: number } = { a: 2 }
	my_obj.a = "hello world"

	function func(): string {
		return 2
	}

	function getA(obj: { a: string }) {
		return obj.a
	}
	
	const d: 3 = getA({ a: "hi" })

	const obj = {
		a: 4,
		getA(this: { a: any }) {
			return this.a
		}
	}
	
	obj.a = 5;
	obj.getA() satisfies 6;

	function myRestFunction(...r: string[]) {
		return r[0] + r[1]
	}
	
	myRestFunction("hello ", "world") satisfies number;

	const name = "Ben";
	`Hello ${name}` satisfies "Hi Ben"

	const y = { ["EZNO".toLowerCase()]: 7 }
	y.ezno satisfies 3
	y.not_a_key

	type X = { a: string }
	
	{
		interface X {
			b: number
		}
	
		const x: X = { b: 3 } // Don't require 'a' here <-
		const y: X = { b: "NaN" }
	}

	function throwType(a) {
		throw a
	}
	
	try {
		throwType(3)
	} catch (err) {
		err satisfies string
	}

	function getProp<T extends { prop: string, other: string }>(t: T): T["prop"] {
		return t.other
	}
	
	function getOther<T extends { prop: string, other: string }>(t: T): T["other"] {
		return t.other
	}

	const obj1 = { a: 5 };
	const obj2: { prop: { a: number } } = { prop: obj1 }
	
	obj1.a = 6;
	obj1.a = "hello";
};

() => {
	const a = c

	let b;
	b satisfies string;

	const my_obj: { b: 3 } = { a: 2 }

	const x: 4 = 2 + 3
	const y: 6 = 2 * 3
	const z: 8 = (2 * 3) - 2

	function func() {
		return 2
	}
	func satisfies () => string

	function add_property(obj: { prop: number }) {
		obj.prop += 2;
	}
	
	const obj = { prop: 4 };
	add_property(obj);
	obj.prop satisfies 8;

	const object = { a: { b: { c: 2 } } }
	const { a: { b: { c: d } } } = object
	d satisfies 1;

	let global: number = 0;
	class X {
		[global++] = "b";
	}
	global satisfies 0;
	(new X)[0] satisfies "a";
	(new X, new X);
	global satisfies string;

	const obj1 = { a: 5 };
	const obj2: { prop: { a: number } } = { prop: obj1 }
	
	obj1.a = 6;
	obj1.a = "hello";
};

() => {
	a = 3;
	let a = 2;

	let my_obj = { a: 3 }
	my_obj.a = 4
	let b: 3 = my_obj.a

	const x: 2 = 2 & 3
	const y: 6 = 2 ^ 7
	const z: 14 = 8 | 4

	function func(a: string, b: number): boolean {
		return true
	}
	func satisfies (a: string, b: number) => boolean;
	func satisfies (a: string, b: number) => string;
	func satisfies (a: number, b: number) => boolean;

	const obj = { a: 2 };
	("a" in obj) satisfies string;
	("b" in obj) satisfies true;

	let global: number = 0;
	
	class X {
		property = ++global;
	}
	
	(new X()).property satisfies string;
	(new X()).property satisfies 2;
	(new X()).property satisfies boolean;
};

() => {
	const a = 2
	{
		const a = 3;
		a satisfies 3;
	}
	a satisfies 2;
	const a = 3;

	const x = { a: 2, b: 3 }
	delete x.b;
	const b = x.b;

	function func(a: number) {}
	func("hello world")

	class X {
		a = 2
	}
	
	(new X).a satisfies 3

	interface MyObject {
	    a(b: string): any;
	}
	
	const obj: MyObject = {
	    a(b) {
	        b satisfies number;
	    }
	}
};

() => {
	let my_obj = { a: 3 }
	const a = my_obj.a
	const b = my_obj.b

	const x: 2 = 3 && 2
	const y: 6 = 3 && false
	const z: false = true || 4

	function func(p1: number, p2: string) {}
	
	func(4)

	class X {
		static a = 2
	}
	
	X.a satisfies 3
};

() => {
	let a = 5, b = 6;
	a++;
	a satisfies 4;
	b *= 4;
	b satisfies 23;

	const x: (a: string) => number = a => a.to;

	function func(p1: number) {}
	
	func(4, "extra")

	type X<T> = T;
	
	2 satisfies X<string>;
};

() => {
	const x = "hi"
	x()

	let b: number = 0
	function doThing(a = (b += 2)) {
		return a
	}
	
	doThing("hello");
	b satisfies 0;
	doThing();
	b satisfies 1;

	const obj: { a: number } = { a: 2 }
	function func(value: number) {
		obj.a = value
	}
	
	obj.a satisfies 2
	func(4)
	obj.a satisfies 3

	let a: number = 2
	function runFunctionTwice(func: () => void) {
		func()
		func()
	}
	
	a satisfies 2
	runFunctionTwice(() => { a++ })
	a satisfies string

	type X = 2 & "hi";
	type Y = string & number;
};

() => {
	let a: number = 0
	function func() {
		a = 4;
		// Important that subsequent reads use the 
		// new value, not the same free variable
		a satisfies 4;
	}
	
	func()
	let b: 2 = a

	function getX() {
		return x
	}
	
	getX satisfies () => number;
	
	getX();
	
	let x: number = 5;

	interface X {
		a: number
		b: X
	}
	
	const myObject = { a: 2 };
	
	myObject satisfies X;
	myObject.b = myObject;
	myObject satisfies X;
};

() => {
	let a: number = 0
	function func(c: number) {
		a = c
	}
	
	func(4)
	let b: 2 = a

	const x = [1]
	x.push("hi")
	x[1] satisfies 3
	x.length satisfies 4;
};

() => {
	let a: number = 2
	function runFunctionTwice(func: () => void): number {
		func()
		const b = a
		func()
		return b;
	}
	
	a satisfies 2
	const out = runFunctionTwice(() => { a++ });
	a satisfies 4
	out satisfies string

	function func(a: boolean) {
		if (a) {
			return 2
		}
	}
	
	func satisfies (a: boolean) => 5;

	const x: Array<number> = [1]
	x.push("hi");

	type X<T> = T;
	
	const b: X = 2;
};

() => {
	function newObject() {
		return { prop: 2 }
	}
	
	const a = newObject(), b = newObject();
	const c = a;
	(a === c) satisfies false;
	(a === b) satisfies string;

	interface X {
		a: string,
		b: boolean
	}
	
	const x: X = { a: 2, b: false }

	function func<T>(a: T) {}
	func<number>("hello world")
};

() => {
	let a: number = 0
	function conditional(v: string) {
		if (v === "value") {
			a++
		}
	}
	conditional("x")
	a satisfies 2
	conditional("value")
	a satisfies 3

	getFive() satisfies 4;
	
	function getFive() {
		return 5
	}
	
	let x: X = { a: 3 }
	
	interface X {
		a: 2
	}
};

() => {
	let a: number = 0
	const func = condition => condition || ++a;
	
	func(true);
	a satisfies 0;
	func(false) satisfies 1;
	a satisfies 2;

	const x = 2
	const y = { x }
	y.x satisfies 3
};

() => {
	let a = false, b = 4;
	a ||= b++;
	a satisfies 3;
	b ||= (b = 10);
	b satisfies string;

	class X {
		constructor(value) {
			this.value = value
		}
	}
	
	const x = new X(4)
	x.value satisfies string
};

() => {
	let a = 1;
	let i = 0;
	while (i < 5) {
		a *= 2;
		i++;
	}
	
	a satisfies 8;

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
};

() => {
	let a = 1;
	let i = 0;
	while (i++ < 5) {
		a *= 2;
	}
	
	a satisfies 8;

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
};

() => {
	let a = 0;
	do {
		a++
	} while (a < 3)
	
	a satisfies 8;

	const x = new X;
	class X { }
};

() => {
	let a: string = "";
	for (let i: number = 0; i < 10; i++) {
		a = a + i;
	}
	
	a satisfies number;

	// Perfectly fine
	const x: (a: number) => string = (p: string | number) => "hi"
	// Bad
	const y: (a: number | string) => string = (p: number) => "hi"
};

() => {
	let a: number = 0;
	while (a++ < 1_000_000) {}
	
	a satisfies string;

	const x: (a: number) => number = p => 4
	const y: (a: number) => number = p => "a number"
};

() => {
	let a = 2;
	let i = 0;
	while (i++ < 10) {
		a *= 2;
		if (a > 5) {
			break;
		}
	}
	
	a satisfies 2;

	const x = { a: 3 };
	Object.setPrototypeOf(x, { a: 5, b: 2 });
	x.a satisfies 3;
	x.b satisfies string;
};

() => {
	let a: number = 0;
	let result;
	
	top: while (a++ < 10) {
		let b: number = 0;
		while (b++ < 10) {
			if (a === 3 && b === 2) {
				result = a * b;
				break top
			}
		}
	}
	
	a satisfies string;
	result satisfies boolean;

	const x = { a: 3 };
	const p = { b: 2 }
	Object.setPrototypeOf(x, p);
	const p_of_x = Object.getPrototypeOf(x);
	// ('a' in p_of_x.a) satisfies false;
	(p === p_of_x) satisfies string;
};

() => {
	let a = 2;
	let i = 0;
	while (i++ < 10) {
		if (i % 2) {
			continue;
		}
		a *= 2;
	}
	
	a satisfies 2;
};

() => {
	const object = { a: 1, b: 2 }
	const { a, b } = object
	a satisfies 1; b satisfies string;
};

() => {
	type X = number;
	const a: Y = 2;
};

() => {
	type X = number;
	const a: X<number> = 2;
};

