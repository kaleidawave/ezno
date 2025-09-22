### As casts

> Disabled normally, allowed for these tests. Provides TSC compatibility and because narrowing not implemented (including secret feature)

```ts
declare let global: any;

5 as boolean;
global satisfies boolean;
(global as string) satisfies number;
```

- Cannot cast 5 to boolean
- Expected boolean, found any
- Expected number, found string

### Non-null assertions

> TODO this currently only works on conditionals

```ts
declare const global: { property?: string };

global.property satisfies string | undefined;
global.property! satisfies number;
```

- Expected number, found string
