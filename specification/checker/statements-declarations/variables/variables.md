#### Variable declarations

```ts
const x: number = 2
const y: string = 2
const z: object = 4
```

- Type 2 is not assignable to type string
- Type 4 is not assignable to type object

#### Variable assignment constraints

```ts
let x: number = 3
x = "hello world"
```

- Type "hello world" is not assignable to type number

#### Variable references

```ts
const a = 3
const b: string = a
```

- Type 3 is not assignable to type string

#### Variable updates registered

```ts
let a = 2
a = "hello world"
a satisfies number
```

- Expected number, found "hello world"

#### Variable references does not exist

```ts
const exists = 2;
nexists
```

- Could not find variable 'nexists' in scope

#### Assigning before declaration

```ts
a = 3;
let a = 2;
```

- Cannot assign to 'a' before declaration

#### Assignment to non-existent variable

```ts
doesNotExist = 4;
```

- Cannot assign to unknown variable 'doesNotExist'

#### Variable declared twice

```ts
const a = 2
{
	const a = 3;
	a satisfies 3;
}
a satisfies 2;
const a = 3;
```

- Cannot redeclare variable 'a'

#### Variable shadowing

> TODO maybe should test loops, functions, function parameters etc...

```ts
const a = 2
{
	const a = 3;
	a satisfies 2;
}
```

- Expected 2, found 3

#### Unintialised variables are undefined

> Might be a usage warning at some point

```ts
let b;
b satisfies string;
```

- Expected string, found undefined

#### TDZ in statements

```ts
let first = second;
let second = 2;
```

- Variable 'second' used before declaration
