> This is an Ezno feature, thanks to events

> #TODO in a function as well

### Delete properties

```ts
const x = { a: 2, b: 3 }
delete x.b;
const b = x.b;
```

- No property 'b' on { a: 2 }

### Un-delete-able property

```ts
const x: { a?: number } = { a: 4 };
// Fine
delete x.a;

const y: { a: number } = { a: 4 };
// Bad
delete y.a;

const z = {};
Object.defineProperty(z, "a", { value: 4 });
delete z.a;
```

- Cannot delete from object constrained to { a: number }
- Cannot delete from non-configurable property
