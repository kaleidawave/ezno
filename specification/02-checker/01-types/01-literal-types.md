In TypeScript there are several types matching exact values

- `number`. e.g. `4`
- `string`. e.g. `'hi'`
- `boolean`. e.g. `true` and `false`
- `undefined`
- `Symbol`

> `null` is under [[08-special-objects/overview.md]]

### Implementation

Each is `Constant` in `terms.rs`

Some constants e.g. `0` have defined `TypeId`s and are reused and referenced at places in the compiler

### To implement

- Big integers