All functions have three properties
1. Type parameters (generic)
2. Parameters
3. Return type

#### Kinds
##### `FunctionKind::Known`

```ts
const x = function (obj: { prop: number }) => {
	obj.prop = 1;
}

x // <- known [...]
```

- May give a more accurate return type during application

##### `FunctionKind::InputOutput`

```ts
console.log // <-- InputOutput

// created with `@InputOutput` decorator
class Console { 
	@InputOutput
	log(...args: Array<any>): void
}
```

- If it takes object with properties, it's side effects are reading those properties

With the decorator

```ts
class Console { 
	@InputOutput
	log();
	// Above is equivalent to (aka if no argument on decorator then use property key)
	@InputOutput("log")
	log()
}
```

##### `FunctionKind::Constant`

```ts
Math.log // <-- Constant

// created with `@Constant` decorator
class Math { 
	@Constant
	sin(x: number): number
}
```

Other than that it is slightly gimmicky. However since release the `Known` side effects and application have got more advanced and are able to do at lot of these things. On the other hand event application is more overhead.

> A future idea is that plugins could inject custom functions. Maybe there could be a custom `read_file` macro that reads and returns a file contents at compile time. Although there might be more ergonomic way to do something like this in the future.

##### `FunctionKind::Unknown`

```ts
function runCallback(callback: (a: string) => string) {
	callback // <- unknown	
}
```

- Most functions which might refer to anything
- May have side effects

##### You can try these with `debug_type`

```ts
debug_type(console.log);

debug_type(Math.sin);

{
	debug_type(function (obj: { prop: number }) => {
		obj.prop = 1;
	});
}

{
	function runCallback(callback: (a: string) => string) {
		debug_type(callback)
	}
}
```

### Kind
There is also the behavior. This decides what happens with `this`, `super` & `new ...`

1. `this` binding
2. `super` etc
3. Called with `new`
