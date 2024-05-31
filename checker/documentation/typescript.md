## Implementation differences
- Variable types are used as reassignment constraints, rather than the absolute value of the reference
- No properties or way to call `any`. `any` annotations are treated as an unknown and have narrower constrains on when used on parameter position 

## What is the same
- Syntax for type annotations

## New diagnostics
- TDZ
- Assignment to union
- Object constraints
- New warnings
- Other things where `any` is returned and now something is used that 

## Diagnostics missing
- Assignment to variables with inferred constraints

## Additional things
👀
