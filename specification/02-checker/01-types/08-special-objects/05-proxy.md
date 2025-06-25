### Implementation

`Proxy` is a special member of `SpecialObject`. It contains two fields

- A `TypeId` to the wrapped object
- A `TypeId` to the trap object

They are created by the `Proxy` constructor which has special behavior. #TODO-link

Several functions check for this trap and delegate to function calling

- Property access
- Property assignment
- Function calling

> #TODO there are others here

### Proxy get

```ts
const proxy1 = new Proxy({ a: 2 }, { 
	get(target: { a: number }, prop: string, receiver) {
		if (prop === "a") return target["a"] + 1;
	}
});

proxy1.a satisfies string;
```

- Expected string, found 3

### Proxy set

```ts
let lastSet: string = "";
const proxy1 = new Proxy({ a: 2 }, {
	set(target: { a: number }, prop: string, value: number, receiver) {
		lastSet = prop;
	}
});

proxy1.a = 6;
lastSet satisfies boolean;
```

- Expected boolean, found "a"

### Proxy handler fallthrough

```ts
const obj = { a: 2 };
const proxy1 = new Proxy(obj, {	});

proxy1.a = 6;
obj.a satisfies 6;
proxy1.a satisfies string;
```

- Expected string, found 6

### Proxy subtyping

```ts
const proxy1 = new Proxy({}, { 
	get(_target, prop, receiver) { return prop } 
});

proxy1 satisfies { a: "a", b: "b" };
proxy1 satisfies { c: "d" };
```

- Expected { c: "d" }, found Proxy [ {}, { get: (_target: any, prop: any, receiver: any) => any } ]

> #TODO list limitations

#### Implementation

#TODO-link to the condition in `subtyping.rs`

### Proxy across functions

> Works because of event constant function application

```ts
function makeObservable(
	obj, 
	cb: (kind: string, prop: string, value: any) => void
) {
	return new Proxy(obj, {
		get(on, prop: string, _rec) {
			cb("get", prop, on[prop])
		},
		set(on, prop: string, _value, _rec) {
			cb("set", prop, on[prop])
		},
	})
}

let r = null;
const value = makeObservable({ a: 1 }, (k, p, v) => {
	r = { k, p, v };
});

r satisfies null;
value.a = 2;
r satisfies string;
```

- Expected string, found { k: "set", p: "a", v: 1 }
