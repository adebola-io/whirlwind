# A Tree-Walk Interpreter.

> STATUS: <b>DRAFT.</b>

This file explains the technical details of a tree-walk interpreter for Whirlwind. The interpreter will be used both in the REPL and temporarily as the main translator for Whirlwind programs, before the WASM compiler is ready.

The intepreter will be a struct that will expose a `.run()` method that takes in a `Standpoint` and executes it, returning the exit code of the program. Execution will start at the `main()` function in the entry module.

If the main function does not adhere to the expectation of an entry function i.e.:

-  the function is not public,
-  the function has parameters
-  the function has a return type

, then interpretation should fail with an `IntermediateError`.

Before the first statement is executed, some checks and arrangements will be done to boost the performance of the interpreter.

## Pruning

The standpoint will be traversed with the help of a `Retriever` to determine all the reachable functions and values, and every other function will be dropped. Reachable functions, function expressions and methods (together grouped as `Callables`) will be stored in an array and accessed using indices.

## Data Structures

The interpreter will have its own set of structures and IRs derived from the standpoint.

The `TypedExpression` and `TypedStatement` structs are expanded to `InterpreterExpression`, and `InterpreterStatement`, respectively. These two new structs are compact representations of the previous structures, with additions of special, optimized variants, which include:

-  Numeric literals with true, computed values to be used at runtime, including:

   -  `Int64Expression(i64)`
   -  `Int32Expression(i32)`
   -  `Float32Expression(f32)`
   -  `Float64Expression(f64)`

-  `ExternalCallExpression(usize)`, which is a call expression to an external function. The interpreter does not support dynamic calls, and it will fail at build time if the function cannot be found. The `usize` is the index of the external function in the `Externals` vector.

-  `FloatOpExpression`, which is a series of operations on values that are floating point numbers. It has the structure:

   ```rs
   struct FloatOpExpression {
    values: Vec<InterpreterExpression>,
    operators: Vec<BinOperator>
   }
   ```

   The list of operators is always one less than the number of values.

-  `IntOpExpression`, which is a series of operations on values that are integers, with the same structure as above.

-  `NestedAccessExpression`, which consists of a root model instance and a list of indices leading from it to a nested property. For example, instead of the access expression `a.b.c.d.e` being a subtree of nodes, it would be:
   ```rs
   struct NestedAccessExpression {
      main: Box<IntermediateExpression>,
      offsets: Vec<usize> // nested offsets into the instance.
   }
   ```

A `Callable` which represents a named function, function expression or method. It contains nothing more than a name and the list of statements to run. Declarations of functions, types, models and interfaces within

## Optimizations

A few of the total optimizations slated for codegen in the WASM compiler will also be used here:

-  Only statements with a possible side effect will be run. i.e. literals and single identifier statements will be ignored.
-  Constants are folded into single expression literals.
-  RHS expressions and expression statements that evaluate to `never` should cause every subsequent statement in the current block to be ignored.
-  If a statement is a variable declaration with only one reference, it is
