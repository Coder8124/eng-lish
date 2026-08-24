# 5. Declarations

Functions, classes, and packages. These appear at the top level of a program.

## 5.1 Functions

```ebnf
function = "To" identifier [ "with" parameters ] [ "returning" return-type ] ":"
           block "End" "." ;
parameters = [ "a" | "an" ] type identifier { "and" [ "a" | "an" ] type identifier } ;
return-type = [ "a" | "an" ] type | "nothing" ;
```

```
To double with a standard number number returning a standard number:
    Give back number * 2.
End.

To greet with a text name:
    output name.
End.

To sayHello:
    output "Hello!".
End.
```

The colon and the closing `End.` are both required. Parameters are separated by `and`, and each carries its own type.

Leaving off `returning` means the function returns nothing. (In [beginner mode](06-beginner-mode.md) it means something different — the return type is worked out from the code.)

`nothing` is only a return type. It cannot be used as the type of a variable or a parameter.

A sentence beginning with `To` is only read as a function definition when the word after the name is `with`, `returning`, or `:`. Otherwise it is read as an ordinary statement.

## 5.2 Classes

```ebnf
class = "Define" [ "a" | "an" ] "kind" "called" identifier
        [ "that" "extends" identifier ]
        "with the following" ":"
        { property | constructor | method }
        "End kind" "." ;

property = "Property" identifier "is" [ "a" | "an" ] type "." ;

constructor = "To create" [ "with" parameters ] ":" block "End create" "." ;

method = "To" identifier [ "with" parameters ] [ "returning" return-type ] ":"
         block "End" "." ;
```

```
Define a kind called Counter with the following:
    Property count is a standard number.

    To create with a standard number initial:
        Set count to initial.
    End create.

    To increment returning nothing:
        Add 1 to count.
    End.

    To getValue returning a standard number:
        Give back count.
    End.
End kind.

Let counter be a Counter created with 0.
Ask counter to increment.
output the result of asking counter to getValue.
```

Points worth spelling out:

- `with the following:` is required, and `the following` is a single keyword.
- The body of a class may contain **only** properties, a constructor, and methods. Ordinary statements are not allowed there.
- The constructor is the method named `create`, and it closes with `End create.` rather than `End.`
- **Properties are in scope inside methods automatically.** There is no `self` or `this`. A method refers to `count` directly.
- Because of that, a constructor parameter **must not** share its name with the property it sets. Name it something else, as `initial` is above.
- `called` and `that` are not keywords. They are matched by name, and remain usable as identifiers elsewhere.

### Inheritance

```
Define a kind called Dog that extends Animal with the following:
```

A class that extends another receives the parent's properties and methods.

## 5.3 Packages

```ebnf
import = "use" text-literal "." ;
mode = "use" "beginner" "." ;
```

Both forms appear at the top of a program, before anything else.

```
use "numeric".
use beginner.
```

Note that a package name is quoted and `beginner` is not — `use beginner.` is a mode switch, not an import. See [chapter 6](06-beginner-mode.md).

### How a package is found

Given `use "numeric".`, an implementation looks in this order and takes the first hit:

1. `<folder of the program>/numeric.eng`
2. `./packages/numeric/numeric.eng`
3. `./packages/numeric/main.eng`
4. `~/.eng-lish/packages/numeric/numeric.eng`
5. `~/.eng-lish/packages/numeric/main.eng`

Candidates 2 and 3 are relative to the directory the compiler was **run from**, not the one the program lives in. In practice this means the bundled packages resolve only when you run `englishc` from the root of the repository.

Imports are followed recursively, and a package that has already been loaded is not loaded twice, so two packages may safely use each other.

### What a package may contain

A package is a `.eng` file holding functions and classes. **Top-level statements in a package are ignored** when it is imported — only its functions and classes are merged into the importing program. A package cannot run code at import time.

The packages that ship with eng-lish are `numeric`, `algorithm`, `geometry`, `strings`, `math`, `game`, and `neural`.

### Installing a package

```
englishc install https://github.com/someone/some-package
```

This clones the repository into `~/.eng-lish/packages/`, under the last part of the URL, where step 4 above will find it.
