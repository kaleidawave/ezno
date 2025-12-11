> TODO move to specification

## Expressions

### References

```ts
x
```

```
VariableReference(
    "x",
    0..1,
)
```

### Literals

```ts
5.6;
'test';
true;
```

```
Expression(
    MultipleExpression(
        NumberLiteral(
            5.6,
            0..3,
        ),
    ),
)
Expression(
    MultipleExpression(
        StringLiteral(
            "test",
            Single,
            5..11,
        ),
    ),
)
Expression(
    MultipleExpression(
        BooleanLiteral(
            true,
            13..17,
        ),
    ),
)
```

### Parenthesised

```ts
(45)
```

```
Parenthesised(
    MultipleExpression(
        NumberLiteral(
            45.0,
            1..3,
        ),
    ),
    0..4,
)
```

### Multiple

```ts
(45, 2)
```

```
Parenthesised(
    MultipleExpression(
        BinaryOperation {
            lhs: NumberLiteral(
                45.0,
                1..3,
            ),
            operator: Comma,
            rhs: NumberLiteral(
                2.0,
                5..6,
            ),
            position: 1..6,
        },
    ),
    0..7,
)
```

### Operators

<!-- TODO -->

### Function argument

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
                5.0,
                12..13,
            ),
        ),
        Standard(
            NumberLiteral(
                6.0,
                15..16,
            ),
        ),
        Standard(
            NumberLiteral(
                7.0,
                18..19,
            ),
        ),
    ],
    is_optional: false,
    position: 0..20,
}
```

### Spread function argument

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

## Variable fields

### `name`

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

### `array`

```ts
let [x, y, z] = null;
let [x,,z] = null;
let [x, ...y] = null;
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
)
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
)
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
)
```

### `object`

```ts
let { x } = null;
let { x = 3 } = null;
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
)
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
                                                3.0,
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
)
```

## For loops

<!-- TODO -->

## Type annotations

<!-- TODO -->

## Strings

### Advanced: string escape new line sequence

> Not sure if positions are okay?

```typescript
"abc\
def"
```

```
StringLiteral(
    "abcdef",
    Double,
    0..10,
)
```

### Advanced: string escape with character

> Not sure if positions are okay?

```typescript
"abc\ndef"
```

```
StringLiteral(
    "abc\ndef",
    Double,
    0..10,
)
```

### Template literal string 

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
