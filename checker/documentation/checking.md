## There are two *types of checking* when type checking a program

### `type` >= `type` (via `syntax`)
These happen in a few places
- Function parameters
- Variable declaration (initial value)
- Variable initial assignment and re- assignment
- Operator operands (can be considered functions)

This is implemented in `types/subtyping`

#### Specialising/substituting
This subtype checking also doubles as a way to specialise generic types that were not or could not be specified as a specific arguments. This is implicitly / inferred, so not explicit arguments.

### `type` querying

Rather than checking whether types are subtypes of some base type, these are concerned with trying to find one specific property of a base type
- Property access (and destructuring)
- Is callable (the parameters can be checked)

> These could be implemented using the first method. Property access could be implemented by checking and substituting using `{ [name]: T }`, but doesn't

## Checking a program
Starts with a list of statements.

Before any of the statements are checked, they are first *hoisted*.

### Hoisting
Both types and variables can be used before their declaration position in the source. Before checking statements, a group of passes 'lifts' certain information.

There are three stages to hoisting.

(1) Firstly
- Interface and type alias **names** are declared

(2) Now that types *mostly* are resolved
- Interfaces properties are attached to the previously declared types
- Variable and function names are declared with their types
   	- *Variables and functions may be used as free variables over, thus declared before(3) at this point*

(3) In the final stage
- Functions are checked
   	- Function bodies are checked
   	- Values that are closed over are found
   	- Inferred parameters are calculated
   	- Return type synthesised
