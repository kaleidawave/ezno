Currently implementing:

> This file is for work-in-progress and can help separating features that are being implemented to regressions

### Iteration

#### Destructuring with iterating

```ts
const [x, y, z] = {
    [Symbols.iterator]() {
        let i = 0;
        return {
            next() {
                if (i++ === 4) return { done: true, value: i };
                return { done: false, value: i };
            },
            return() {
                return { done: true };
            },
        };
    }
};

({ x, y, z }) satisfies string;
```

- Expected string, found { x: 1, y: 2, z: 3 }
