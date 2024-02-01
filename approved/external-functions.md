# External Functions.

> STATUS: <b>50% Implemented.</b>

This file details the introduction of a mechanism to define foreign functions within the language scope. External functions will either be resolved:

-  statically at compile time,
-  or dynamically at runtime as WebAssembly import statements.

It would enable seamless integration with external functionality from the host environment. It also allows the definition of a boundary between the core and host libraries, and lower level functionality.

## Syntax:

This feature will introduce:

-  a `import` keyword to the list of tokens,
-  `Statement:ImportDeclaration` and `ImportedFunctionDeclaration` to the syntax AST
-  `TypedStmnt:TypedImportDeclaration` and `TypedImportedFunctionDeclaration` to the bound module AST

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

Having this feature would fit seamlessly with scope management and typechecking, since only the signature is needed for the compiler frontend.

However, generic external functions should result in type errors. Have not decided whether monomorphization will happen, but if it does, we cannot create two or more variants of a foreign function.

## Code Generation:

The resource string determines when the function should be resolved. Strings that are prefixed with `internal:` will have the functions replaced with WASM-native functions at compile time.

Others will resolve to WASM import statements.

### Example:

```ts
import "console" {
    "log" as function log(offset: Int32, length: Int32)
}
```

would compile to:

```wasm
(import "console" "log" (func $_log (param i32 i32)))
```

## Additional Considerations:

<strike>
### Default Values?

Whether or not there should be a way to define fallback functions if the external resource providing it cannot be resolved. This would just be completing the function signature with a body, like so:

</strike>
