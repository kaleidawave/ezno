Arrays are regular objects with a prototype pointing to the `Array` class

#### Array push

```ts
const x = [1];
x.push("hi");
x[1] satisfies 3;
x.length satisfies 4;
```

- Expected 3, found "hi"
- Expected 4, found 2

#### Array pop

```ts
const myArray = [6, "hi"]
myArray.pop() satisfies 3;
myArray.length satisfies 1;
```

- Expected 3, found "hi"

#### Array push restriction

```ts
const x: Array<number> = [1]
x.push("hi");
```

- Argument of type \"hi\" is not assignable to parameter of type number

#### Array property checking

```ts
const numbers1: Array<number> = [1, 2, "3"],
		numbers2: Array<string> = ["hi", "3"],
		numbers3: Array<string> = 4;
```

- Type [1, 2, "3"] is not assignable to type Array\<number>
- Type 4 is not assignable to type Array\<string>


> TODO link to events