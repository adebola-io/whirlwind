# Ideas for Syntax 
- Equality comparison with `==`.
- Implicit Return
- Reference Comparison with `is`.
- Atoms can be re-exported using Export Import syntax.
- Variables should be declared using `:=` syntax.
- Optional parameters, that become `Maybe`s in the function code.
- Rest parameters that become Arrays in function code.
- Extendable Classes
- Operator overloading with Traits. e.g.
- ```ts

  trait PartialEq<T> {
    Equals(Other: T): Boolean
  }

  class EmployeeList implements PartialEq<This> {
    Items: ArrayOf<Employee>;

    EmployeeList() {
      This.Items = [];
    }
  
    [PartialEq.Equals](Other: This): Boolean {
      return This.Items == Other.Items
    }
  }
  
  ```
- Records: immutable key-value stores.
  ```ts
  
  record Messages {
    1 = "Hello, World!"
    2 = "Good day!"
  }
  
  ```

# Ideas for Semantics
- Test statements should only be allowed at the top level of the module.
- Getters and Setters for object properties.
- Garbage collection

## Ideas for Runtime 
- Copy on write strings.  

# Ideas for Examples
- A Guess the Number Game.
- A graphic user interface.
- A Web server.
- A full stack web app.

# Online Repl

# Ideas for Core Library 
- A markdown, xml and JSON parser in `Core.Data`.
- A title case and a sentence case method for strings.
- A random items picker and a shuffler for arrays.
- A `Maybe` object and an `Outcome` class.
  ```ts
  import Core.Internals.{Nil, IsNil};

  /// Creates a `Maybe` with no value. 
  export function None<T>(): Maybe<T> {
    return Maybe(Nil)
  }
  
  # Creates a `Maybe` with an internal value.
  export function Some<T>(Value: T): Maybe<T> {
    return Maybe(Value)
  }

  /// A value that may or may not exist.
  export class Maybe<T> {
    Value: T;

    Maybe(Value: T) {
      This.Value = Value;
    }
 
    IsNone(): Boolean {
      return IsNil(This.Value);
    }

    IsSome(): Boolean {
      return !IsNil(This.Value);
    }
   
    Unwrap(): T {
      if IsNil(This.Value) {
        Panic("Called Unwrap on a None Value");
      } else {
        return This.Value;
      }
    }

    UnwrapOr(Value: T) {
      if IsNil(This.Value) {
        return Value;
      } else {
        return This.Value;
      }
    }
  }
  ```
- An `EventEmitter` class in `Core.Utils`.
- A `Record.ToMap()` method that turns a record to a mutable map.
- `Core.Net.Request` to make HTTP requests.
