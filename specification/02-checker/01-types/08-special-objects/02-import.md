> #TODO-link for full module stuff

### `import.meta`

> Unfortunately because of bundling affecting `url` (and thus `resolve`) we cannot have known results so just `string`

```ts
import.meta.url satisfies number;
import.meta.resolve("./lib/helper.js") satisfies string;

import.meta.env.production satisfies boolean;
```

- Expected number, found string
- Expected boolean, found string | undefined
