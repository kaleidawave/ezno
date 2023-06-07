# Ezno Checker

Contains type checking logic

See [oxc_type_synthesis](https://github.com/Boshen/oxc) for the code that connects the logic to the OXCs AST. The `synthesis` folder contains similar logic for ezno-parser but is currently out of date and doesn't compile.

## Testing

Set `EZNO_DEBUG` to any value to trace diagnostic information from the `crate::utils::notify!` macro (In powershell = `$Env:EZNO_DEBUG=1`)
