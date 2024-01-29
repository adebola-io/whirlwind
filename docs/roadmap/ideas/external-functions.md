# External Functions.

> STATUS: <b>0% Implemented.</b>

This file details the introduction of a mechanism to define foreign functions within the language scope. External functions will either be resolved:

-  statically at compile time,
-  dynamically at runtime for the interpreter.
-  or dynamically at runtime as WebAssembly import statements.

It would enable seamless integration with external functionality from other WebAssembly modules or the host environment. It also allows the definition of a boundary between the core and system libraries, and lower level functionality.

## Syntax:

This feature will introduce:

-  a `from` keyword to the list of tokens,
-  `ScopeEntry:ExternalFunction` to the module ambience,
-  `Statement:ExternDeclaration` and `ExternFunctionDeclaration` to the syntax AST
-  `TypedStmnt:TypedExternDeclaration` and `TypedExternFunctionDeclaration` to the bound module AST
-  `InterpreterStmnt::Builtin(usize)` to the interpreter statement list.

External functions are declared as function signatures, basically functional declarations without a body.

The alternative syntaxes will be:

```js
// Approach 1: grouped external functions.
from("resource") {
   /// Adds two integers together.
   public function add(a: Int32, b: Int32) -> Int32;

   /// Exits the program.
   public function exit(code?: Int32);

   // other function signatures.
}

// Approach 2: single external function.
from("resource") function print(message: String);
```

## Semantics

-  **These declarations will only be allowed in the global scope to prevent stories that touch**.
-  **External functions cannot have generic parameters, since they do not belong in the scope of Whirlwind.**

## Resource Types

A valid `resource` string must have the form `type:resource`, where `type` could be:

-  `file`: The path to the resource could either be:
   -  a relative path from the module requesting the resource e.g. `file:../pre.wat`, or
   -  a path from the root of the installation directory, which is represented by an `@` symbol. e.g. `file:@/ext/math.wat`.
-  `http`: This should allow requesting functions from network resources. (It will require a compiler flag to enable.) e.g. `http://web.com/exports`.
-  `env`, to import an item from the host runtime.
-  `builtin`: For the interpreter, this will resolve to a Rust function.

## Semantics:

Having this feature fits seamlessly with scope management and typechecking, since only the signature is needed for the compiler frontend.

However, generic external functions should result in type errors. Have not decided whether monomorphization will happen, but if it does, we cannot create two or more variants of a foreign function.

## Code Generation:

During code generation, external functions will be resolved by the defined compiler loaders, if they exist.

Here the resource name and type will be validated, and invalid resources will halt compilation.

If a function is loaded twice into the same program, it should cause a warning or an error.

## Loaders

The definition about what a resource actually is is intentionally vague to allow flexibility in its implementation. It could be a `.wasm` file, a network url, a builtin library or somthing else.

Loaders should basically be plugins that transform anything to Whirlwind's intermediate representation or directly to webassembly text or bytecode. The details are still hazy.

### Example:

```js
from("file:@/ext/math.wasm") {
    function multiply(a: Int32, b: Int32) -> Int32;
}

from("wasi:env") {
    function print(s: String);
}

function main() {
    var c = multiply(2, 3);
    print(c.toStr());
}
```

### Generated WebAssembly:

```wasm
(import "math" "add" (func $add (param i32 i32) (result i32)))

(import "env" "print" (func $print (param i32)))

(func $main
  (local $c i32)
  (local.get $c)
  (i32.const 1)
  (i32.const 2)
  (call $add)
  (local.set $c)
  (local.get $c)
  (call $print)
)
```

## Additional Considerations:

### Default Values?

Whether or not there should be a way to define fallback functions if the external resource providing it cannot be resolved. This would just be completing the function signature with a body, like so:

```js
from("file:@/ext/math.wasm") {
    /// Function to add two numbers.
    function add(a: Int32, b: Int32) -> Int32 {
        return a * a;
    }
}
```

### Allow other signatures from external resources?

It may or may not be possible to extend the syntax and semantics to support importing models, constants and even modules from external resources.

```js
from("http://github.com/user/package/releases/v1") {
    /// Definition of an external model.
    model User {
        var name: String;
        var age: Int32;
        /// Signature of the constructor.
        new(name: String, age: Int32);
        /// Signature of method.
        public function sayHello() -> String;
    }

    // Definition of external constant.
    const MAGIC_VALUE: Int64;

    /// Definition of a module from the resource.
    module math {
        /// Adds two items together.
        function add(a: Int32, b: Int32) -> Int32;
    };
}
```
