> Some can be implmented by events

We define them in `.d.ts` files

```ts
@Constant("my_constant_function")
declare function myFunc();

assert_equal(getFunctionConstantIdentifier(myFunc), "my_constant_function")
```

> TODO link to object descriptors

#### Mathematics and string operations

> While could be implemented in JS and work with events, it is not worth it

#### Functions for compilation

- Read file #TODO-start
- Read environment variable #TODO-start

#### Testing and debugging functions

These are work in progress, but we can

#### Proxy and RegExp constructors and methods

> #TODO-link for each

#### Function.bind

> #TODO-link for `this`

#### `JSON` methods

> Using `simple-json-parser`


> #TODO-start

#### String operations (constant functions can use `this`)

```ts
"hi".toUpperCase() satisfies number
```

- Expected number, found "HI"

#### Math operations

```ts
Math.cos(0) satisfies 0
Math.sqrt(16) satisfies 1
Math.floor(723.22) satisfies 2;
```

- Expected 0, found 1
- Expected 1, found 4
- Expected 2, found 723
