Some objects in JavaScript have special behaviour that is not possible to re-create in userland JavaScript (aka using operators).

These are represented by the `SpecialObjects` enum.

There are currently several JavaScript item which have a special variant of `Type` and are handled differently in many functions
- [[02-import]] #TODO from `import.meta` as property access can 
- [[03-promise]] has special behaviours for events
- [[05-proxy]] many operations are *trapped*, running functions rather than default object lookup etc
- [[07-regexp]] we can extract groups **and** compute results at check time