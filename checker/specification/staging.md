Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Things

#### Narrowing chains

```ts
export type User = { username: string, password: string };
export type AuthPredicate = (username: string, password: string) => boolean;
export type Auth = User | User[] | AuthPredicate;

function run(auth: Auth)  {
    if (Array.isArray(auth)) {
        auth satisfies number;
    } else if (typeof auth === "function") {
        auth("hi", 5) satisfies string;
    } else {
        auth satisfies boolean;
    }
}
```

- Expected number, found Array\<User>
- Expected 5 bad
- Expected string, found boolean
- Expected boolean, found User

#### String map

```ts
print_type("\"" === '"')
```

- ?