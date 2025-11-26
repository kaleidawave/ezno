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

#### Parenthesised

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

#### Multiple

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

#### `object`

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

### For loops

<!-- TODO -->

### Type annotations

<!-- TODO -->

### Strings

#### Advanced: string escape new line sequence

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

#### Advanced: string escape with character

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
using name1 = value1, name2 = value2
using name3: IDisposable = value3 
await using name4 = value4 
```

```
UsingDeclaration(
    UsingDeclaration {
        is_await: false,
        bindings: [
            UsingBinding {
                name: "name1",
                annotation: None,
                value: VariableReference(
                    "value1",
                    14..20,
                ),
            },
            UsingBinding {
                name: "name2",
                annotation: None,
                value: VariableReference(
                    "value2",
                    30..36,
                ),
            },
        ],
        position: 0..37,
    },
)
UsingDeclaration(
    UsingDeclaration {
        is_await: false,
        bindings: [
            UsingBinding {
                name: "name3",
                annotation: Some(
                    Name(
                        TypeName(
                            "IDisposable",
                        ),
                        50..61,
                    ),
                ),
                value: VariableReference(
                    "value3",
                    64..70,
                ),
            },
        ],
        position: 37..72,
    },
)
UsingDeclaration(
    UsingDeclaration {
        is_await: true,
        bindings: [
            UsingBinding {
                name: "name4",
                annotation: None,
                value: VariableReference(
                    "value4",
                    92..98,
                ),
            },
        ],
        position: 78..99,
    },
)
```

#### `using` in for loop

> #TODO more

```typescript
for (using resource of resources) { }
```

```
ForLoop(
    ForLoopStatement {
        condition: ForOf {
            is_await: false,
            lhs: Using {
                is_await: false,
                annotation: None,
                name: "resource",
            },
            of: VariableReference(
                "resources",
                23..32,
            ),
            position: 5..32,
        },
        inner: Braced(*empty*),
        position: 0..37,
    },
)
```

### Edge cases

#### `satisfies` with

> Any expression-level postfix type syntax for that point

```typescript
x satisfies string && y;
x satisfies string & { length: 2 }; 
```

```
Expression(
    MultipleExpression(
        BinaryOperation {
            lhs: SpecialOperators(
                Satisfies {
                    value: VariableReference(
                        "x",
                        0..1,
                    ),
                    type_annotation: CommonName(
                        String,
                        12..18,
                    ),
                },
                0..18,
            ),
            operator: LogicalAnd,
            rhs: VariableReference(
                "y",
                22..23,
            ),
            position: 0..23,
        },
    ),
)
Expression(
    MultipleExpression(
        SpecialOperators(
            Satisfies {
                value: VariableReference(
                    "x",
                    25..26,
                ),
                type_annotation: Intersection(
                    [
                        CommonName(
                            String,
                            37..43,
                        ),
                        ObjectLiteral(
                            [
                                None(
                                    Decorated {
                                        decorators: [],
                                        on: Property {
                                            name: Identifier(
                                                "length",
                                                48..54,
                                                Public,
                                            ),
                                            type_annotation: NumberLiteral(
                                                2.0,
                                                56..57,
                                            ),
                                            is_readonly: false,
                                            is_optional: false,
                                            position: 48..57,
                                        },
                                        position: 48..57,
                                    },
                                ),
                            ],
                            46..59,
                        ),
                    ],
                    37..59,
                ),
            },
            25..59,
        ),
    ),
)
```

#### LHS of assignment as any expression

> Only a parse error under `"use strict"`

```typescript
func()++;
```

```
UnaryPostfixAssignmentOperation {
    operand: Neither(
        FunctionCall {
            function: VariableReference(
                "func",
                0..4,
            ),
            type_arguments: None,
            arguments: [],
            is_optional: false,
            position: 0..6,
        },
    ),
    operator: UnaryPostfixAssignmentOperator(
        Increment,
    ),
    position: 0..8,
}
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
                            2.0,
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