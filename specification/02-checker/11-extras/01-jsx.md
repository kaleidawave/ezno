JSX support is based on the existance of a `h` function.

> TODO expand

### JSX type

```tsx
function JSXH(tag_name: string, attributes: any, children: any) {
	return { tag_name, attributes, children }
}

const x = <h1 title="Example text">Hello World</h1> satisfies string;
```

- Expected string, found { tag_name: "h1", attributes: { title: "Example text" }, children: ["Hello World"] }

#### Implementation

This looks for a function called `JSXH` function in scope
