#### `fetch`

> Uses external `Promise`

```ts
const resp = await (fetch("/some-endpoint") satisfies string);

resp.ok satisfies number;
```

- Expected string, found Promise\<Response>
- Expected number, found boolean

### Classes
