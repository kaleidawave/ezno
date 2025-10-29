TypeScript implements a few intrinsic types to 

### `Uppercase`

```ts
const a: Uppercase<"something" |"hi"> = "HI";
const b: Uppercase<string> = "hi"
```

- Type \"hi\" is not assignable to type Uppercase\<string>

### `CaseInsensitive`

> Ezno only

```ts
"Hi" satisfies CaseInsensitive<"hi">;
"Hello" satisfies CaseInsensitive<"hi">;

// yeah
type CIWord = "WORD" extends Uppercase<infer T> ? T : never;
"wOrd" satisfies CIWord;
"wood" satisfies CIWord;
```

- Expected CaseInsensitive<"hi">, found "Hello"
- Expected CIWord, found "wood"
