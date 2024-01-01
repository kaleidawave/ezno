Currently implementing:

### Not sure

#### Set property on dependent observed

```ts
function add_property(obj: { prop: number }) {
    obj.prop = 2;
    (obj.prop satisfies 4);
}
```

> Not number

- Expected 4, found 2

### To fix

#### Constant call and operation with a parameter

> An example of the generic constructor type (namely call and operation)

```ts
function floorPlusB(a: number, b: number) {
	return Math.floor(a) + b
}

floorPlusB(100.22, 5) satisfies 8
```

- Expected 8, found 105

#### Calling new on a function

```ts
function MyClass(value) {
	this.value = value
}

new MyClass("hi").value satisfies "hello"
```

- Expected "hello", found "hi"
