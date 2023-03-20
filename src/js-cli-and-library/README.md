# Ezno

This contains a JavaScript edition of [Ezno](https://github.com/kaleidawave/ezno)

## Example

```js
import { build } from "ezno";

const content = "const x = !t ? 4 : 5;";
console.dir(build(content, "input.js"), { depth: 5 })
```

## Commands for building this package

Run from this folder, **not the root**:

```shell
npm run clean
npm run build
```

See `package.json` for the full commands.
