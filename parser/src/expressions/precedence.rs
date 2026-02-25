//! > TODO copy links
//!
//! |Precedence|Associativity|Individual operators|Notes|
//! |---|---|---|---|
//! |18: grouping|n/a|Grouping(x)|[1]|
//! |17: access and call|left-to-right|Member access `x.y`|[2]|
//! |17: access and call|left-to-right|Optional chaining `x?.y`|[2]|
//! |17: access and call|n/a|Computed member accessx[y]|[3]|
//! |17: access and call|n/a|new with argument list `new x(y)`|[4]|
//! |17: access and call|n/a|Function call `x(y)`|[4]|
//! |17: access and call|n/a|`import(x)`|[4]|
//! |16: new|n/a|new without argument list `new x`||
//! |15: postfix operators|n/a|Postfix increment `x++`|[5]|
//! |15: postfix operators|n/a|Postfix decrement `x--`|[5]|
//! |14: prefix operators|n/a|Prefix increment `++x`|[6]|
//! |14: prefix operators|n/a|Prefix decrement `--x`|[6]|
//! |14: prefix operators|n/a|Logical NOT `!x`||
//! |14: prefix operators|n/a|Bitwise NOT `~x`||
//! |14: prefix operators|n/a|Unary plus `+x`||
//! |14: prefix operators|n/a|Unary negation `-x`||
//! |14: prefix operators|n/a|Unary negation `-x`||
//! |14: prefix operators|n/a|`typeof x`||
//! |14: prefix operators|n/a|`void x`||
//! |14: prefix operators|n/a|`delete x`|[7]|
//! |14: prefix operators|n/a|`await x`||
//! |13: exponentiation|right-to-left|Exponentiation `x ** y`|[8]|
//! |12: multiplicative operators|left-to-right|Multiplication `x * y`||
//! |12: multiplicative operators|left-to-right|Division `x / y`||
//! |12: multiplicative operators|left-to-right|Remainder `x % y`||
//! |11: additive operators|left-to-right|Addition `x + y`||
//! |11: additive operators|left-to-right|Subtraction `x - y`||
//! |10: bitwise shift|left-to-right|Left shift `x << y`||
//! |10: bitwise shift|left-to-right|Right shift `x >> y`||
//! |10: bitwise shift|left-to-right|Unsigned right shift `x >>> y`||
//! |9: relational operators|left-to-right|Less than `x < y`||
//! |9: relational operators|left-to-right|Less than or equal `x <= `y||
//! |9: relational operators|left-to-right|Greater than `x > y`||
//! |9: relational operators|left-to-right|Greater than or equal `x >= `y||
//! |9: relational operators|left-to-right|`x in y`||
//! |9: relational operators|left-to-right|`x instanceof y`||
//! |8: equality operators|left-to-right|Equality `x == y`||
//! |8: equality operators|left-to-right|Inequality `x != y`||
//! |8: equality operators|left-to-right|Strict equality `x === y`||
//! |8: equality operators|left-to-right|Strict inequality `x !== y`||
//! |7: bitwise AND|left-to-right|Bitwise AND `x & y`||
//! |6: bitwise XOR|left-to-right|Bitwise XOR `x ^ y`||
//! |5: bitwise OR|left-to-right|Bitwise OR `x \| y`||
//! |4: logical AND|left-to-right|Logical AND `x && y`||
//! |3: logical OR, nullish coalescing|left-to-right|Logical OR `x || y`||
//! |3: logical OR, nullish coalescing|left-to-right|Nullish coalescing operator `x ?? y`|[9]|
//! |2: assignment and miscellaneous|right-to-left|Assignmentx = y|[10]|
//! |2: assignment and miscellaneous|right-to-left|Addition assignment `x += y`||
//! |2: assignment and miscellaneous|right-to-left|Subtraction assignment `x -= y`||
//! |2: assignment and miscellaneous|right-to-left|Exponentiation assignment `x **= y`||
//! |2: assignment and miscellaneous|right-to-left|Multiplication assignment `x *= y`||
//! |2: assignment and miscellaneous|right-to-left|Division assignment `x /= y`||
//! |2: assignment and miscellaneous|right-to-left|Remainder assignment `x %= y`||
//! |2: assignment and miscellaneous|right-to-left|Left shift assignment `x <<= y`||
//! |2: assignment and miscellaneous|right-to-left|Right shift assignment `x >>= y`||
//! |2: assignment and miscellaneous|right-to-left|Unsigned right shift assignment `x >>>= y`||
//! |2: assignment and miscellaneous|right-to-left|Bitwise AND assignment `x &= y`||
//! |2: assignment and miscellaneous|right-to-left|Bitwise XOR assignment `x ^= y`||
//! |2: assignment and miscellaneous|right-to-left|Bitwise OR assignment `x |= y`||
//! |2: assignment and miscellaneous|right-to-left|Logical AND assignment `x &&= y`||
//! |2: assignment and miscellaneous|right-to-left|Logical OR assignment `x ||= y`||
//! |2: assignment and miscellaneous|right-to-left|Nullish coalescing assignment `x ??= y`||
//! |2: assignment and miscellaneous|right-to-left|Conditional (ternary) operator `x ? y : z`|[11]|
//! |2: assignment and miscellaneous|right-to-left|Arrow `x => y`|[12]|
//! |2: assignment and miscellaneous|n/a|`yield x`||
//! |2: assignment and miscellaneous|n/a|`yield* x`||
//! |2: assignment and miscellaneous|n/a|Spread `...x`|[13]|
//! |1: comma|left-to-right|Comma operator `x, y`||
//!
//! Notes:
//!
//! 1. The operand can be any expression.
//! 2. The "right-hand side" must be an identifier.
//! 3. The "right-hand side" can be any expression.
//! 4. The "right-hand side" is a comma-separated list of any expression with precedence > 1 (i.e., not comma expressions). The constructor of a `new` expression cannot be an optional chain.
//! 5. The operand must be a valid assignment target (identifier or property access). Its precedence means `new Foo++` is `(new Foo)++` (a syntax error) and not `new (Foo++)` (a TypeError: (Foo++) is not a constructor).
//! 6. The operand must be a valid assignment target (identifier or property access).
//! 7. The operand cannot be an identifier or a private element access.
//! 8. The left-hand side cannot have precedence 14.
//! 9. The operands cannot be a logical OR `||` or logical AND `&&` operator without grouping.
//! 10. The "left-hand side" must be a valid assignment target (identifier or property access).
//! 11. The associativity means the two expressions after `?` are implicitly grouped.
//! 12. The "left-hand side" is a single identifier or a parenthesized parameter list.
//! 13. Only valid inside object literals, array literals, or argument lists.
//!
//! The precedence of groups 17 and 16 may be a bit ambiguous. Here are a few examples to clarify.
//!
//! - Optional chaining is always substitutable for its respective syntax without optionality (barring a few special cases where optional chaining is forbidden). For example, any place that accepts `a?.b` also accepts `a.b` and vice versa, and similarly for `a?.()`, `a()`, etc.
//! - Member expressions and computed member expressions are always substitutable for each other.
//! - Call expressions and `import()` expressions are always substitutable for each other.
//! - This leaves four classes of expressions: member access, `new` with arguments, function call, and `new` without arguments.
//! - The "left-hand side" of a member access can be: a member access (`a.b.c`), new with arguments (`new a().b`), and function call (`a().b`).
//! - The "left-hand side" of `new` with arguments can be: a member access (`new a.b()`) and new with arguments (`new new a()()`).
//! - The "left-hand side" of a function call can be: a member access (`a.b()`), `new` with arguments (`new a()()`), and function call (`a()()`).
//! - The operand of `new` without arguments can be: a member access (`new a.b`), `new` with arguments (`new new a()`), and `new` without arguments (`new new a`).

// Operator precedences that aren't registered under operator trait
pub(crate) const COMMA_PRECEDENCE: u8 = 1;
pub(crate) const CONDITIONAL_TERNARY_PRECEDENCE: u8 = 2;
pub(crate) const ARROW_FUNCTION_PRECEDENCE: u8 = 2;
pub(crate) const ASSIGNMENT_PRECEDENCE: u8 = 2;
pub(crate) const YIELD_OPERATORS_PRECEDENCE: u8 = 2;
pub(crate) const RELATION_PRECEDENCE: u8 = 10;
pub(crate) const CONSTRUCTOR_WITHOUT_PARENTHESIS_PRECEDENCE: u8 = 16;
pub(crate) const MEMBER_ACCESS_PRECEDENCE: u8 = 17;
pub(crate) const INDEX_PRECEDENCE: u8 = 17;
pub(crate) const FUNCTION_CALL_PRECEDENCE: u8 = 17;
pub(crate) const CONSTRUCTOR_PRECEDENCE: u8 = 17;
pub(crate) const PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE: u8 = 18;
