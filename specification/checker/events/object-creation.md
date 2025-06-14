#### Functions create objects

```ts
function newObject() {
	return { prop: 2 }
}

const a = newObject(), b = newObject();
const c = a;
(a === c) satisfies false;
(a === b) satisfies string;
```

- Expected false, found true
- Expected string, found false
