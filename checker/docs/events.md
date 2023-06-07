# Events
Events track what the program does. Examples include
- Updating a property
- Calling any function (including IO, external)
- Throwing errors

They reference things in the type system. Their are no values, only type references to constants and other poly types.

## Effects and events
The side effects of a function are bundled into the events in the function. Side effects = events that

### Function synthesis and calling
When synthesizing a function, events are recorded. They are then held on the function type. When calling that function, the events are replayed with the current state of the program.

## Using as an IR
The events can be used as an intermediate representation as a a representation of the program in a lowered syntax.

Things events can be used to do after checking
- Detecting unused assignments
- Generating other representations
