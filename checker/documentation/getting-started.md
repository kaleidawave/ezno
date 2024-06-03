Ezno is work in progress. It doesn't currently support all the features of JavaScript and TypeScript **but** it has a fairly large [specification of features that it supports today](../specification/specification.md).

While it **is not worth it trying it on existing codebases at this time** (as it likely will blow up 💥), **you can try out the snippets in the [specification](../specification/specification.md)** and other small pieces of code today.

The best way to try the type checker is on the [web playground](https://kaleidawave.github.io/ezno/playground).

Alternative you can try the checker locally using the `check` command of ezno binary. The simplest way is using `npx`

```shell
npx ezno check file.ts
```

You can also download the binary with `npm install ezno`. Or for the native (non WASM version) you can get it with `cargo install ezno` or on [GitHub releases](https://github.com/kaleidawave/ezno/releases).

---

You can use the `print_type` function to see the type of expressions.

```tsx
const x = 6;
print_type(x + 8)
```

If you find any unexpected exceptions, please [leave an issue 😁](https://github.com/kaleidawave/ezno/issues/new)
