Currently implementing:

### Stuff

#### Object function inference

```ts
interface MyObject {
    a(b: string): any;
}

const obj: MyObject = {
    a(b) {
        print_type(b)
    }
}
```

- TODO?
