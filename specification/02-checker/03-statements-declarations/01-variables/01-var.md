[var](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/var)

### `var`

```ts
s satisfies string;
var s = "hello"
s satisfies number;
```

- Expected string, found undefined
- Expected number, found "hello"

### `var` can be reregistered

```ts
{
	// Fine
	var x = 2;
	x satisfies 2;
	var x = 3;
	x satisfies 3;
}

{
	let b = 2;
	var b = 2;
}
```

- Cannot redeclare variable 'b'
