> Some can be implmented by events

We define them in `.d.ts` files

```ts
@Constant("my_constant_function")
declare function myFunc();

assert_equal(getFunctionConstantIdentifier(myFunc), "my_constant_function")
```

#### Mathematics and string operations

> While could be implemented in JS and work with events, it is not worth it

#### `Object.defineProperty`

#### `Object.seal` etc

> Thanks to https://github.com/kaleidawave/ezno/pull/213

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

