### `typeof` operator

The `typeof` prefix operator returns a string based on its operand

| Type | Result|
| --- | --- |
| [Undefined](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/undefined) | `"undefined"` |
| [Null](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/null) | `"object"` ([reason](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof#typeof_null)) |
| [Boolean](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Boolean) | `"boolean"`|
| [Number](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number) | `"number"` |
| [BigInt](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt) | `"bigint"` |
| [String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String) | `"string"` |
| [Symbol](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol) | `"symbol"` |
| [Function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function) (implements [[Call]] in ECMA-262 terms; [classes](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/class) are functions as well) | `"function"` |
| Any other object | `"object"` |

> Table from [`typeof` on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof)

Here we test some literals as well as a with variable of type `number` (a non-literal type)

```ts
function func() {}

(typeof 5) satisfies "number";
(typeof "hi") satisfies "string";
(typeof func) satisfies "function";

declare let someNumber: number;
(typeof someNumber) satisfies "function";
```

- Expected "function", found "number"

#### Implementation

We extract the `large` type ( #TODO-link) of the synthesised operand. We recursively split if the type is union. We pattern match on `TypeId`, (`TypeId::NUMBER => "number"`) creating new string literal types #TODO-check based on the match.

#TODO-link to code/function