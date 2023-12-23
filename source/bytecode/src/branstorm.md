## Blocking Cyclic Types?

## Representation of Opaque Types in Memory

## Creating an Instance.

-  To create an instance of a model:
-  Load the arguments, in consecutive order, onto the stack, inside the current frame.
-  Create a new call frame, then offset its start index in the stack by the number of arguments, so that the arguments are now values inside the new frame.
-  For optional values that have not been initialized, load them as empty Maybe values into the frame.
-  Create an instance of the model being created.
-  Run the function and store the instance pointer in the ret register.
-  Return from the constructor function.

## Calling a Named Function.

-  Load the arguments, in consecutive order, onto the stack, inside the current frame.
-  Create a new call frame and offset by the number of arguments so that the arguments are within the new frame.
-  For optional values that have not been initialized, load them as empty Maybe values into the frame.
-  Run the function and store its return value in the ret register.
-  Return with [Opcode::Return](./opcode.rs).

## Calling a Method.

-  The same as calling a function, but the `this` register must be loaded first.
