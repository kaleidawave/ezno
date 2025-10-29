### `fetch`

> Uses external `Promise`

```ts
const resp = await (fetch("/some-endpoint") satisfies string);

resp.ok satisfies number;
```

- Expected string, found Promise\<Response>
- Expected number, found boolean

### To implement

Promises should be a special object that store

- A returned type
- A progress of where events are
