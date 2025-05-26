Extending from methods. We have iteration events. Catches certain things, enables more information. > #TODO iteration seems like a lot of overhead and can be reduced

- Array.push etc

This enables `Object` methods like `.values`, based on `for in` so we aren not special casing `Object.values`

```ts
function myObjectValues(obj: object) {
	for (const key in obj) {}
}
```

#TODO
- return
- annotation bypass
- break, continue
