# 6. Beginner mode

```
use beginner.
```

Written at the top of a program, this turns on a smaller, gentler version of the language for people who have not programmed before. It removes the need to write types, and it changes what error messages look like.

The line **must** come before the code it affects, because the compiler switches modes as it reads.

## 6.1 Functions without types

Outside beginner mode, every parameter needs a type. Inside it, they can be left off:

```
use beginner.

To double x returning a standard number:
    Give back x * 2.
End.

To addUp x and y returning a standard number:
    Give back x + y.
End.

To sayHello:
    output "Hello!".
End.
```

The `returning` clause may also be left off, but see the limitation in 6.5 before doing so in a function that gives something back.

The `with` is dropped along with the types, and parameters are still separated by `and`.

```ebnf
beginner-function = "To" identifier [ identifier { "and" identifier } ] ":"
                    block "End" "." ;
```

## 6.2 Calling with `of`

Beginner mode adds a shorter way to call a function:

```
output double of 5.
output addUp of 3 and 4.
```

`the result of double with 5` still works and means the same thing. The `of` form is only available in beginner mode.

```ebnf
beginner-call = identifier "of" arguments ;
```

## 6.3 Functions named after keywords

A handful of keywords may be used as function names in beginner mode, because they are the obvious names for what a beginner is writing:

`add` · `subtract` · `multiply` · `divide` · `result` · `value` · `kind` · `property`

```
use beginner.

To add x and y returning a standard number:
    Give back x + y.
End.

output add of 2 and 3.
```

## 6.4 How types get worked out

Beginner mode does not remove types — it works them out for you.

- A parameter with no written type takes its type from the **first call** to that function. Every later call **must** agree, or the compiler reports a conflict.
- A function with no `returning` clause takes its return type from what it gives back.
- A parameter that is never pinned down by any call defaults to `standard number`.
- A function that never gives anything back returns nothing.

Every inferred type **must** be resolved before code is generated. This matters to implementers: the internal `Inferred` type must never reach the code generator.

## 6.5 A limitation in the current implementation

Return-type inference does not yet work when the answer **depends on a parameter whose type was also inferred**. These two compile:

```
To five:
    Give back 5.
End.

To greet name:
    output name.
End.
```

but this one does not:

```
To double x:
    Give back x * 2.
End.
```

It fails with `Type::Inferred should be resolved before codegen`, because the parameter's type arrives from the call site after the return type has already been fixed.

Until this is fixed, **write the `returning` clause whenever a beginner-mode function gives back something computed from its parameters**:

```
To double x returning a standard number:
    Give back x * 2.
End.
```

This is a bug in the reference implementation, not a rule of the language. A conforming implementation **should** infer the return type here.

## 6.6 Friendlier errors

Both parse errors and type errors are reported in a gentler format:

```
Oops! Line 3 has a problem.
Try: ...
```

instead of the terser message used outside beginner mode. The compiler looks for the literal line `use beginner.` in the source before parsing, so this formatting applies even to errors that happen before the mode flag has been set.

## 6.7 What beginner mode does not change

Everything else is the same language. Types, statements, loops, classes, and the `End.` rules are all unchanged, and a beginner-mode program may use any part of the language described in the earlier chapters.
