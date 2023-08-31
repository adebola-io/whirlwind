# Ideas for Syntax

-  Equality comparison with `==`.
-  Implicit Return
-  Reference Comparison with `is`.
-  Atoms can be re-exported using public use syntax.
-  Variables should be declared using `:=` syntax.
-  Optional parameters, that become `Maybe`s in the function code.
-  Rest parameters that become Arrays in function code.
-  Extendable Classes
-  Operator overloading with traits. e.g.
-  Records: immutable key-value stores.

# Ideas for Semantics

-  Test statements should only be allowed at the top level of the module.
-  Getters and Setters for object properties.
-  Only function expressions can have type-inferred parameters.
-  Prevent instantiation of classes without a constructor.
-  Extended classes must call be super() method in their constructor.
-  Constructors are functions with the `new` identifier.
-  Generate error if class properties are not definitely assigned in the constructor.
-  `_` represents special catchall/undefined type.
-  All blocks return either an expression or the `_` type.
-  `_` cannot be assigned to a variable, only used in wrappers like `Outcome` and `Maybe`.
-  Garbage collection

## Ideas for Runtime

-  Copy on write strings.

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
