Ezno is work in progress. It doesn't currently support all the features of JavaScript and TypeScript **but** it has a fairly large [specification of features that it supports today](../specification/specification.md). While it isn't worth it trying it on existing codebases ATM as it likely will blow up. You can try out the snippets in the specification and other small pieces of code today.

You can try the `check` command of ezno using `npx`

```shell
npx ezno check file.ts
```

Or download the binary with `npm install ezno`, `cargo install ezno` or on [GitHub releases](https://github.com/kaleidawave/ezno/releases).

You can use the `print_type` function to see the type of expressions.

```tsx
const x = 6;
print_type(x + 8)
```

If you find any unexpected exceptions, please leave an issue 😁
