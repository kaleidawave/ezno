Events can be run conditionally. Additionally function calls can be run.

## Conditions

### Known conditional event result

```ts
let a: number = 2;
function update(cond: bool) {
	if (cond) a = 3;
}

a satisfies 2;
update(false);
a satisfies 2;
update(true);
a satisfies 4;
```

- Expected 4, found 3

### Unknown conditional events result

```ts
declare let cond: bool;

let a: number = 2;
function update(cond: bool) {
	if (cond) a = 3;
}

a satisfies 2;
update(cond);
a satisfies 2;
```

- Expected 2, found 2 | 3

## Running functions

### Across

```ts
let a = 0;
function invoke(f: () => void) {
	f()
}

a satisfies 2;
invoke(() => { a = 3 });
a satisfies 4;
```

- Expected 4, found 3

### Tree shaking ⭐

> #TODO move

With this analyis we can remove class properties, we can track things across conditionals. We have an event marking that a function was called.

This works through `Proxy` etc

> Note this is to do with events, types aid it but type annotations do not enable this existance
