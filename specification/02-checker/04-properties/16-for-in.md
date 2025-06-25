This is based on iteration event application

### For-in fixed object

```ts
let properties: string = "";
for (const property in { a: 1, b: 2, c: 3 }) {
	properties += property;
}
properties satisfies boolean;
```

- Expected boolean, found "abc"

### For-in non fixed object

> TypeScript anonymous object annotations do not guarantee ordering and the subtyping rules allow for the RHS to have more
> properties than defined

```ts
declare const myObject: { a: 1, b: 2, c: 3 };

let properties: string = "";
for (const property in myObject) {
	properties += property;
}
properties satisfies boolean;
```

- Expected boolean, found string
