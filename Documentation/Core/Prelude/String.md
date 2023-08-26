# Static Methods

# Instance Methods

-  ## At

```whirl
    Test {
        Str := "This is a world premiere!";
        Assert(Str.At(3)).Equals("s")
    }
```

-  ## Append

```whirl
    Test {
        Str := "Hello";
        Str.Append(", world!");

        Assert(String).Equals("Hello, world!");
    }
```

-  ## Chars
-  ## Clear

```whirl
    Test {
        Str := "This is a string.";
        Str.Clear();

        Assert(Str.Len()).Equals(0);
    }
```

-  ## Concat

-  ## Compare
-  ## Contains
-  ## Len
-  ## Reverse
-  ## Slice
-  ## Replace
-  ## Trim
-  ## Pad
-  ## UpperCase
-  ## LowerCase
