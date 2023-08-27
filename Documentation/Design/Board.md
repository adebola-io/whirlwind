# Ideas for Syntax

-  Equality comparison with `==`.
-  Implicit Return
-  Reference Comparison with `is`.
-  Atoms can be re-exported using Export Import syntax.
-  Variables should be declared using `:=` syntax.
-  Optional parameters, that become `Maybe`s in the function code.
-  Rest parameters that become Arrays in function code.
-  Extendable Classes
-  Operator overloading with traits. e.g.
-  ```wrl
   trait PartialEq<T> {
      Equals(Other: T): Boolean;
   }

   class EmployeeList implements PartialEq<This> {
      items: ArrayOf<Employee>;

      EmployeeList() {
         this.items = [];
      }

      [PartialEq.Equals](Other: This): Boolean {
         return this.items == other.items;
      }
   }
   ```

-  Records: immutable key-value stores.

   ```wrl

   record Messages {
     1 = "Hello, World!"
     2 = "Good day!"
   }

   ```

# Ideas for Semantics

-  Test statements should only be allowed at the top level of the module.
-  Getters and Setters for object properties.
-  Only function expressions can have type-inferred parameters.
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

   ```wrl
   import Core.Internals.{Nil, isNil};

   /// Creates a `Maybe` with no value.
   export function none<T>(): Maybe<T> {
     return Maybe(Nil);
   }

   /// Creates a `Maybe` with an internal value.
   export function some<T>(Value: T): Maybe<T> {
     return Maybe(Value);
   }

   /// A value that may or may not exist.
   export class Maybe<T> {
     value: T;

     Maybe(value: T) {
       this.value = value;
     }

     isNone(): Boolean {
       return isNil(this.value);
     }

     isSome(): Boolean {
       return !isNil(this.value);
     }

     unwrap(): T {
       if isNil(this.value) {
         Panic("Called Unwrap on a None Value");
       } else {
         return this.value;
       }
     }

     unwrapOr(value: T) {
       if IsNil(this.value) {
         return value;
       } else {
         return this.value;
       }
     }
   }
   ```

-  An `EventEmitter` class in `Core.Utils`.
-  A `Record.ToMap()` method that turns a record to a mutable map.
-  `Core.Net.Request` to make HTTP requests.
