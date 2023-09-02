# Ideas for Syntax

-  Equality comparison with `==`.
-  Implicit Return
-  Reference Comparison with `is`.
-  Atoms can be re-exported using public use syntax.
-  Variables should be declared using `:=` syntax.
-  Optional parameters, that become `Maybe`s in the function code.
-  Rest parameters that become Arrays in function code.
-  Extendable Classes
-  Records: immutable key-value stores.

# Ideas for Semantics

-  Test statements should only be allowed at the top level of the module.
-  Getters and Setters for object properties.
-  Only function expressions can have type-inferred parameters.
-  Prevent instantiation of classes without a constructor.
-  Subclasses must call the `super()` method for each class in their constructor.
-  Constructors are functions with the `new` identifier.
-  Variables cannot be used until they are deterministically assigned. A caveat is variables that implement `Core.Internal.Default`.
-  Generate error if class properties are not definitely assigned in the constructor.
-  Polymorphism: Multiple generic traits can be implemented. e.g.

   ```wrl
   class Value extends Index {
       [Index<String>.Access](value: String): String {
           //
       }

       [Index<Number>.Index](accessor: Number): Number {
           //
       }
   }
   ```

-  `_` represents special catchall/undefined type.
-  All blocks return either an expression or the `_` type.
-  Types can be automatically upcasted to superclasses and union types.
-  Types can be downcasted using the `as` keyword to an inclusive set of supertypes. Downcasting should cause panic if it is incorrect at runtime.
-  `_` cannot be assigned to a variable, only used in wrappers like `Outcome` and `Maybe`.
-  Garbage collection?

## Ideas for Runtime

-  Copy on write strings.
-  In cases where values are deterministically assigned, do not assign defaults.
-

# Ideas for Examples

-  A Guess the Number Game.
-  A graphic user interface.
-  A Web server.
-  A full stack web app.

# Online Repl

# Ideas for Core Library

-  A markdown, xml and JSON parser in `Core.Data`.
-  A title case and a sentence case method for strings.
-  A random items picker and a shuffler for arrays.
-  A `Maybe` object and an `Outcome` class.
-  An `EventEmitter` class in `Core.Utils`.
-  A `Record.ToMap()` method that turns a record to a mutable map.
-  `Core.Net.Request` to make HTTP requests.
