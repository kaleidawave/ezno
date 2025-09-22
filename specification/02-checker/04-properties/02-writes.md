### Set property with key

```ts
const obj: { a?: number, b?: number } = { a: 2 }

function setProperty(key: "a" | "b", value: number) {
	obj[key] = value;
}

setProperty("b", 6)
obj satisfies string;
```

- Expected string, found { a: 2, b: 6 }

### Assigning to types as keys

```ts
const obj = { a: 1, b: 2, c: 3 };
obj satisfies { [s: string]: number };
obj satisfies { [s: string]: boolean };
```

- Expected { [string]: boolean }, found { a: 1, b: 2, c: 3 }

### Through another variable

```ts
const obj1 = { a: 5 };
const obj2: { prop: { a: number } } = { prop: obj1 }

obj1.a = 6;
obj1.a = "hello";
```

- Type "hello" does not meet property constraint number

> As would violate any usage of `obj2`

### Assinging to non existent property

> Allowing this could break objects passed to functions

```ts
const obj = { prop: 1 };
// Fine
obj.non_existent = 6;

function func(param: { prop: number }) {
	param.notProp = 5;
}
```

- Cannot write to non-existent property 'notProp'
