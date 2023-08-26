# Strings
Strings are sequences of characters.

-  ## At

```whirl
    Test "Tests String.At method" {
        Str := "This is a world premiere!";
        Assert(Str.At(3)).Equals("s")
    }
```

-  ## Append

```whirl
    Test "Tests String.Append method" {
        Str := "Hello";
        Str.Append(", world!");

        Assert(Str).Equals("Hello, world!");
    }
```

-  ## Chars
-  ## Clear

```whirl
    Test "Tests String.Clear Method" {
        Str := "This is a string.";
        Str.Clear();

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
