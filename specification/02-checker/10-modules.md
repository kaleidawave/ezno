#### `import.meta`

> Thanks to [#141](https://github.com/kaleidawave/ezno/pull/141)

> TODO link to special object

#### Import and export named

also *imports work with and without extensions*

```ts
import { PI } from "./constants.ts";
import { PI as otherPI, "non identifier" as a } from "./other";

PI satisfies string;
otherPI satisfies boolean;
a satisfies 8;

// in constants.ts
export const PI = 4;

// in other.ts
export const PI = 22 / 7;
const private = 2;
export { private as "non identifier" }
```

- Expected string, found 4
- Expected boolean, found 3.142857142857143
- Expected 8, found 2

#### Imports are constant

```ts
import { PI } from "./constants";
PI += 2;

// in constants.ts
export let PI = 4;
```

- Cannot assign to constant

#### Import default

```ts
import PI from "./pi";
PI satisfies string;

// in pi.ts
export default 4;
```

- Expected string, found 4

#### Import type

```ts
import { MyNumber } from "./types";
2 satisfies MyNumber;

// in types.ts
export type MyNumber = string;
```

- Expected MyNumber, found 2

#### Import type and variable

```ts
import { MyNumber } from "./types";
2 satisfies MyNumber;
MyNumber satisfies boolean;

// in types.ts
export type MyNumber = string;
export const MyNumber = 6;
```

- Expected MyNumber, found 2
- Expected boolean, found 6

#### Export let

```ts
import { counter, incrementCounter } from "./mutable";

counter satisfies string;
incrementCounter();
counter satisfies 3;
incrementCounter();
counter satisfies string;

// in mutable.ts
export let counter: number = 2;
export function incrementCounter() {
	counter++
}
```

- Expected string, found 2
- Expected string, found 4

#### Import star

```ts
import * as the from "./many";

the satisfies string;

// in many.ts
export const a = 2, b = 3, c = 4;
```

- Expected string, found { a: 2, b: 3, c: 4 }

#### Import from non existent file

```ts
import { a } from "./two";

console.log(a.prop);

// in one.ts
export const a = 2;
```

- Cannot find ./two

#### Import where export does not exist

```ts
import b, { a } from "./export";

console.log(a.prop);

// in export.ts
export const c = 2;
```

- Cannot find default export from module './export'
- a not exported from ./export

#### Import conflicts with existing name

```ts
import { x } from "./export1";
import x, { z } from "./export2";

// in export1.ts
export const x = 1;

// in export2.ts
const y = 2;

export default y;
export const z = 2;
```

- Cannot import using conflicting name

#### Import from invalid file

```ts
import { a } from "./export";

console.log(a.prop);

// in export.ts
export default const;
```

- Found reserved identifier

#### Only synthesis module once

```ts
import { a } from "./export1";
import { b } from "./export2";

(a === b) satisfies string;

// in export1.ts
export { the as a } from "./base"

// in export2.ts
export { the as b } from "./base"

// in base.ts
export const the = ((4 satisfies 1),3);
```

- Expected 1, found 4
- Expected string, found true

> The fact the `Expected 1, found 4` only occurs once means that the module was only synthesised once

#### Use export in scope

```ts
export const x = 2;
x satisfies 3;
```

- Expected 3, found 2

#### Imports don't leak non exports

```ts
import { x } from "./exports"
console.log(y)

// in exports.ts
export const x = 2;
const y = "122LH"
```

- Could not find variable 'y' in scope

#### Import side effect

> Don't take this as permission to do this

```ts
import { x } from "./export";
import "./side_effect";

x satisfies number;

// in side_effect.ts
import { x } from "./export";

x satisfies string;

x.b = x.a + 2;

// in export.ts
export const x = { a: 2 };
```

- Expected string, found { a: 2 }
- Expected number, found { a: 2, b: 4 }

#### Import package

> Yay doesn't require type definition to be shipped!!

```ts
import { mean_gravity } from "earth";

mean_gravity satisfies 2;

// in node_modules/earth/package.json
{
	"main": "constants.js"
}

// in node_modules/earth/constants.js
export const mean_gravity = 9.806;
```

- Expected 2, found 9.806
