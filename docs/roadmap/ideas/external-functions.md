# External Functions.

> STATUS: <b>0% Implemented.</b>

This file details the introduction of a mechanism to define foreign functions within the language scope. External functions will either be resolved:

-  statically at compile time,
-  or dynamically at runtime as WebAssembly import statements.

It would enable seamless integration with external functionality from other WebAssembly modules or the host environment. It also allows the definition of a boundary between the core and system libraries, and lower level functionality.

## Syntax:

This feature will introduce:

-  a `import` keyword to the list of tokens,
-  `ScopeEntry:ExternalFunction` to the module ambience,
-  `Statement:ExternDeclaration` and `ExternFunctionDeclaration` to the syntax AST
-  `TypedStmnt:TypedExternDeclaration` and `TypedExternFunctionDeclaration` to the bound module AST
-  `InterpreterStmnt::Builtin(usize)` to the interpreter statement list.

External functions are declared as function signatures, basically functional declarations without a body.

The alternative syntaxes will be:

```js
// Approach 1: grouped external functions.
import "resource" {
   /// Adds two integers together.
   "add" as function add(a: Int32, b: Int32) -> Int32

   /// Exits the program.
   "exit" as function exit(code?: Int32)

   // other function signatures.
}
```

## Semantics

-  **These declarations will only be allowed in the global scope to prevent stories that touch**.
-  **External functions cannot have generic parameters, since they do not belong in the scope of Whirlwind.**

```ts
import "console" {
    "log" as function log(offset: Int32, length: Int32)
}
```

would compile to:

```wasm
(import "console" "log" (func $_log (param i32 i32)))
```

## Semantics:

Having this feature would fit seamlessly with scope management and typechecking, since only the signature is needed for the compiler frontend.

However, generic external functions should result in type errors. Have not decided whether monomorphization will happen, but if it does, we cannot create two or more variants of a foreign function.

## Code Generation:

During code generation, external functions will be resolved by the defined compiler loaders, if they exist.

Here the resource name and type will be validated, and invalid resources will halt compilation.

If a function is loaded twice into the same program, it should cause a warning or an error.

### Example:

```js
import "console" {
    "log" as function log(s: String)
}

function main {
    var c = multiply(2, 3)
    log(c.toStr())
}
```

## Additional Considerations:

<strike>
### Default Values?

Whether or not there should be a way to define fallback functions if the external resource providing it cannot be resolved. This would just be completing the function signature with a body, like so:

</strike>
