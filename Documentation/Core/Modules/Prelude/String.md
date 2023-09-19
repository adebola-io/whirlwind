# Strings

Strings are sequences of characters.

-  ## At

```whirl
    test "Tests String.At method" {
        str := "This is a world premiere!";
        Assert(Str.At(3)).Equals("s")
    }
```

-  ## Append

```whirl
    test "Tests String.Append method" {
        str := "Hello";
        str.Append(", world!");

        Assert(Str).Equals("Hello, world!");
    }
```

-  ## Chars
-  ## Clear

```whirl
    test "Tests String.Clear Method" {
        str := "This is a string.";
        str.Clear();

        Assert(Str.Length()).Equals(0);
    }
```

-  ## Concat

-  ## Compare
-  ## Contains
-  ## Length
-  ## Reverse
-  ## Slice
-  ## Replace
-  ## Trim
-  ## Pad
-  ## UpperCase
-  ## LowerCase
