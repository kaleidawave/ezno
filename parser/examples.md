> TODO move to specification

## Examples

### Expressions

#### References

```ts
x
```

```
VariableReference(
    "x",
    0..1,
)
```

#### Literals

```ts
5.6;
'test';
true;
```

```
Module {
    hashbang_comment: None,
    items: [
        Expression(
            MultipleExpression(
                NumberLiteral(
                    Number(
                        5.6,
                    ),
                    0..3,
                ),
            ),
        ),
        Expression(
            MultipleExpression(
                StringLiteral(
                    "test",
                    Single,
                    5..11,
                ),
            ),
        ),
        Expression(
            MultipleExpression(
                BooleanLiteral(
                    true,
                    13..17,
                ),
            ),
        ),
    ],
    span: 0..19,
}
```

#### Parenthesised

```ts
(45)
```

```
Parenthesised(
    MultipleExpression(
        NumberLiteral(
            Number(
                45.0,
            ),
            1..3,
        ),
    ),
    0..4,
)
```

#### Multiple

```ts
(45, 2)
```

```
Parenthesised(
    MultipleExpression(
        BinaryOperation {
            lhs: NumberLiteral(
                Number(
                    45.0,
                ),
                1..3,
            ),
            operator: Comma,
            rhs: NumberLiteral(
                Number(
                    2.0,
                ),
                5..6,
            ),
            position: 1..6,
        },
    ),
    0..7,
)
```

#### Operators

<!-- TODO -->

#### Function argument

```ts
console.log(5, 6, 7);
```

```
FunctionCall {
    function: PropertyAccess {
        parent: VariableReference(
            "console",
            0..7,
        ),
        is_optional: false,
        property: Standard {
            property: "log",
            is_private: false,
        },
        position: 0..11,
    },
    type_arguments: None,
    arguments: [
        Standard(
            NumberLiteral(
                Number(
                    5.0,
                ),
                12..13,
            ),
        ),
        Standard(
            NumberLiteral(
                Number(
                    6.0,
                ),
                15..16,
            ),
        ),
        Standard(
            NumberLiteral(
                Number(
                    7.0,
                ),
                18..19,
            ),
        ),
    ],
    is_optional: false,
    position: 0..20,
}
```

#### Spread function argument

```ts
console.table(...a);
```

```
FunctionCall {
    function: PropertyAccess {
        parent: VariableReference(
            "console",
            0..7,
        ),
        is_optional: false,
        property: Standard {
            property: "table",
            is_private: false,
        },
        position: 0..13,
    },
    type_arguments: None,
    arguments: [
        Spread(
            VariableReference(
                "a",
                17..18,
            ),
            14..18,
        ),
    ],
    is_optional: false,
    position: 0..19,
}
```

### Variable fields

#### `name`

```ts
let x;
```

```
Variable(
    Exportable {
        is_exported: false,
        item: VariableDeclaration {
            kind: Let,
            declarations: [
                VariableDeclarationItem {
                    name: None(
                        Name(
                            Standard(
                                "x",
                                4..5,
                            ),
                        ),
                    ),
                    type_annotation: None,
                    expression: None,
                    position: 4..5,
                },
            ],
            position: 0..5,
        },
    },
)
```

#### `array`

```ts
let [x, y, z] = null;
let [x,,z] = null;
let [x, ...y] = null;
```

```
Module {
    hashbang_comment: None,
    items: [
        Variable(
            Exportable {
                is_exported: false,
                item: VariableDeclaration {
                    kind: Let,
                    declarations: [
                        VariableDeclarationItem {
                            name: None(
                                Array {
                                    members: [
                                        None(
                                            Name(
                                                Name(
                                                    Standard(
                                                        "x",
                                                        5..6,
                                                    ),
                                                ),
                                                None,
                                                None,
                                            ),
                                        ),
                                        None(
                                            Name(
                                                Name(
                                                    Standard(
                                                        "y",
                                                        8..9,
                                                    ),
                                                ),
                                                None,
                                                None,
                                            ),
                                        ),
                                        None(
                                            Name(
                                                Name(
                                                    Standard(
                                                        "z",
                                                        11..12,
                                                    ),
                                                ),
                                                None,
                                                None,
                                            ),
                                        ),
                                    ],
                                    spread: None,
                                    position: 4..13,
                                },
                            ),
                            type_annotation: None,
                            expression: Some(
                                Null(
                                    16..20,
                                ),
                            ),
                            position: 4..20,
                        },
                    ],
                    position: 0..20,
                },
            },
        ),
        Variable(
            Exportable {
                is_exported: false,
                item: VariableDeclaration {
                    kind: Let,
                    declarations: [
                        VariableDeclarationItem {
                            name: None(
                                Array {
                                    members: [
                                        None(
                                            Name(
                                                Name(
                                                    Standard(
                                                        "x",
                                                        27..28,
                                                    ),
                                                ),
                                                None,
                                                None,
                                            ),
                                        ),
                                        None(
                                            None,
                                        ),
                                        None(
                                            Name(
                                                Name(
                                                    Standard(
                                                        "z",
                                                        30..31,
                                                    ),
                                                ),
                                                None,
                                                None,
                                            ),
                                        ),
                                    ],
                                    spread: None,
                                    position: 26..32,
                                },
                            ),
                            type_annotation: None,
                            expression: Some(
                                Null(
                                    35..39,
                                ),
                            ),
                            position: 26..39,
                        },
                    ],
                    position: 22..39,
                },
            },
        ),
        Variable(
            Exportable {
                is_exported: false,
                item: VariableDeclaration {
                    kind: Let,
                    declarations: [
                        VariableDeclarationItem {
                            name: None(
                                Array {
                                    members: [
                                        None(
                                            Name(
                                                Name(
                                                    Standard(
                                                        "x",
                                                        46..47,
                                                    ),
                                                ),
                                                None,
                                                None,
                                            ),
                                        ),
                                    ],
                                    spread: Some(
                                        SpreadDestructuringField(
                                            Name(
                                                Standard(
                                                    "y",
                                                    52..53,
                                                ),
                                            ),
                                            49..53,
                                        ),
                                    ),
                                    position: 45..54,
                                },
                            ),
                            type_annotation: None,
                            expression: Some(
                                Null(
                                    57..61,
                                ),
                            ),
                            position: 45..61,
                        },
                    ],
                    position: 41..61,
                },
            },
        ),
    ],
    span: 0..63,
}
```

#### `object`

```ts
let { x } = null;
let { x = 3 } = null;
```

```
Module {
    hashbang_comment: None,
    items: [
        Variable(
            Exportable {
                is_exported: false,
                item: VariableDeclaration {
                    kind: Let,
                    declarations: [
                        VariableDeclarationItem {
                            name: None(
                                Object {
                                    class_name: None,
                                    members: [
                                        None(
                                            Name(
                                                Standard(
                                                    "x",
                                                    6..7,
                                                ),
                                                None,
                                                None,
                                                6..7,
                                            ),
                                        ),
                                    ],
                                    spread: None,
                                    position: 4..9,
                                },
                            ),
                            type_annotation: None,
                            expression: Some(
                                Null(
                                    12..16,
                                ),
                            ),
                            position: 4..16,
                        },
                    ],
                    position: 0..16,
                },
            },
        ),
        Variable(
            Exportable {
                is_exported: false,
                item: VariableDeclaration {
                    kind: Let,
                    declarations: [
                        VariableDeclarationItem {
                            name: None(
                                Object {
                                    class_name: None,
                                    members: [
                                        None(
                                            Name(
                                                Standard(
                                                    "x",
                                                    24..25,
                                                ),
                                                None,
                                                Some(
                                                    NumberLiteral(
                                                        Number(
                                                            3.0,
                                                        ),
                                                        28..29,
                                                    ),
                                                ),
                                                24..29,
                                            ),
                                        ),
                                    ],
                                    spread: None,
                                    position: 22..31,
                                },
                            ),
                            type_annotation: None,
                            expression: Some(
                                Null(
                                    34..38,
                                ),
                            ),
                            position: 22..38,
                        },
                    ],
                    position: 18..38,
                },
            },
        ),
    ],
    span: 0..40,
}
```

### For loops

<!-- TODO -->

### Type annotations

<!-- TODO -->

### Strings

#### Advanced: string escape new line sequence

```typescript
"abc\
def"
```

```
StringLiteral(
    "abcdef",
    Double,
    0..8,
)
```

#### Advanced: string escape with character

```typescript
"abc\ndef"
```

```
StringLiteral(
    "abc\ndef",
    Double,
    0..9,
)
```

#### Template literal string 

```typescript
`x ${a} b`
```

```
TemplateLiteral(
    TemplateLiteral {
        tag: None,
        parts: [
            (
                "x ",
                MultipleExpression(
                    VariableReference(
                        "a",
                        5..6,
                    ),
                ),
            ),
        ],
        final_part: " b",
        position: 0..10,
    },
)
```

### Extras

#### JSX

> TODO script etc, top level HTML option etc

```tsx
<h1 title="Example text">Hello World</h1>
```

```
JSXRoot(
    Element(
        JSXElement {
            tag_name: "h1",
            attributes: [
                Static(
                    "title",
                    "Example text",
                    10..24,
                ),
            ],
            children: Children(
                [
                    TextNode(
                        "Hello World",
                        25..36,
                    ),
                ],
            ),
            position: 0..41,
        },
    ),
)
```

### [Resource management](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/using)

#### `using`

```typescript
using name1 = value1, name2 = value2;
```

```
Module {
    hashbang_comment: None,
    items: [
        UsingDeclaration(
            UsingDeclaration {
                is_await: false,
                bindings: [
                    (
                        "name1",
                        VariableReference(
                            "value1",
                            14..20,
                        ),
                    ),
                    (
                        "name2",
                        VariableReference(
                            "value2",
                            30..36,
                        ),
                    ),
                ],
                position: 0..36,
            },
        ),
        AestheticSemiColon(
            36..37,
        ),
    ],
    span: 0..38,
}
```

#### `using` in for loop

```typescript
for (using resource of resources) { }
```

```
ForLoop(
    ForLoopStatement {
        condition: ForOf {
            keyword: Some(
                Using,
            ),
            variable: None(
                Name(
                    Standard(
                        "resource",
                        11..19,
                    ),
                ),
            ),
            of: VariableReference(
                "resources",
                23..32,
            ),
            is_await: false,
            position: 5..32,
        },
        inner: Braced(
            Block(
                [],
                34..37,
            ),
        ),
        position: 0..37,
    },
)
```

### Partial syntax

#### Missing variable name

```typescript
const = 2;
```

```
Variable(
    Exportable {
        is_exported: false,
        item: VariableDeclaration {
            kind: Const,
            declarations: [
                VariableDeclarationItem {
                    name: None(
                        Name(
                            Marker(
                                Marker(
                                    0,
                                    PhantomData<ezno_parser::variable_fields::VariableIdentifier>,
                                ),
                                6..6,
                            ),
                        ),
                    ),
                    type_annotation: None,
                    expression: Some(
                        NumberLiteral(
                            Number(
                                2.0,
                            ),
                            8..9,
                        ),
                    ),
                    position: 6..9,
                },
            ],
            position: 0..9,
        },
    },
)
```

#### Missing variable value

```typescript
const variable = ;
```

```
Variable(
    Exportable {
        is_exported: false,
        item: VariableDeclaration {
            kind: Const,
            declarations: [
                VariableDeclarationItem {
                    name: None(
                        Name(
                            Standard(
                                "variable",
                                6..14,
                            ),
                        ),
                    ),
                    type_annotation: None,
                    expression: Some(
                        Marker {
                            marker_id: Marker(
                                0,
                                PhantomData<ezno_parser::expressions::Expression>,
                            ),
                            position: 16..17,
                        },
                    ),
                    position: 6..17,
                },
            ],
            position: 0..17,
        },
    },
)
```