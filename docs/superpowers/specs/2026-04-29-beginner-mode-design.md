# Beginner Mode Design

**Date:** 2026-04-29
**Goal:** Make eng-lish accessible to kids ages 10–13 learning programming, while remaining a scripting language and keeping all existing `.eng` files unchanged.

---

## Problem

Two friction points block young learners:

1. **Error messages** are technical compiler output — kids can't act on them.
2. **Concepts arrive too fast** — function definitions with full type annotations and verbose call expressions overwhelm beginners before they understand what a function is.

Variable declarations (`let x be a standard number with value 10.`) stay unchanged — types are educational and worth learning early.

---

## Solution: `use beginner.` file header

A single header line opts a file into beginner mode. All changes are additive and scoped to that file. Every existing `.eng` file is unaffected.

```
use beginner.
```

- Must be the first line of the file
- Compatible with subsequent `use "package".` statements
- A file is either fully in beginner mode or fully in standard mode — no mixing

---

## Section 1: Activation

```
use beginner.
use "numeric".

output "Hello, World!".
```

The `use beginner.` line must appear before any code or other `use` statements. After that, all three beginner-mode features described below are active.

---

## Section 2: Simplified Function Definitions

Type annotations on parameters and return type are optional in beginner mode.

**Standard mode:**
```
To double with a standard number x returning a standard number:
    let result be a standard number with value x.
    Multiply result by 2.
    Give back result.
End.

To greet with a text name returning nothing:
    output "Hello, ".
    output name.
End.
```

**Beginner mode:**
```
To double x:
    let result be a standard number with value x.
    Multiply result by 2.
    Give back result.
End.

To greet name:
    output "Hello, ".
    output name.
End.
```

**Multiple parameters** use `and`:
```
To add x and y:
    Give back x.
End.
```

**Zero-parameter functions:**
```
To sayHello:
    output "Hello!".
End.
```

Rules:
- Signature form: `To <name> <param> and <param> ...:`
- `Give back` for returning a value — unchanged
- `End.` to close — unchanged
- `returning nothing` is dropped; just omit `Give back`
- Untyped parameters take their type from the argument passed at the call site. If the same function is called with conflicting types (e.g., once with a number, once with text), the compiler produces a beginner-friendly error on the second call site. If a function is defined but never called, untyped parameters are left unchecked.
- Zero-argument functions are defined as `To <name>:` and called with `Call <name>.` (void) or `the result of <name>` (returning a value) — the standard forms, since `<name> of` with no argument is ungrammatical

---

## Section 3: Simplified Function Calls

**Standard mode (expression):**
```
let y be a standard number with value the result of double with 5.
output the result of combine with "Hello" and " World".
```

**Beginner mode (expression):**
```
let y be a standard number with value double of 5.
output combine of "Hello" and " World".
```

Pattern: `<name> of <arg>` (single arg) or `<name> of <arg> and <arg>` (multiple args).

**Unchanged in beginner mode:**

Void calls are already readable and stay the same:
```
Call greet with "World".
Call sleep with 500.
```

Object method calls are already natural and stay the same:
```
Ask c to increment.
output the result of asking c to getValue.
```

Zero-argument calls keep their standard form — both for built-ins and user-defined zero-arg functions:
```
let line be a text with value the result of readLine.
let greeting be a text with value the result of buildGreeting.
```

Rules:
- `the result of <name> with <args>` → `<name> of <args>` in beginner mode
- `and` separates multiple arguments in both forms
- `Call` statements and `Ask`/`asking` method calls are unchanged
- The `of` keyword is only valid as a function call shorthand inside beginner-mode files

---

## Section 4: Friendlier Error Messages

In beginner mode, every compiler error is rewritten with three parts:
1. What went wrong (plain English, no jargon)
2. Where it is (line number)
3. What to try (concrete suggestion or corrected snippet)

**Standard mode (current):**
```
error[E001]: type mismatch: expected standard number, found text
  --> examples/hello.eng:3:12
```

**Beginner mode:**
```
Oops! Line 3 has a problem.
  You're using "hello" (text) where a number is expected.
  Try: Did you mean to write a number instead of "hello"?
```

**More examples:**

Undefined variable:
```
Oops! Line 5 has a problem.
  'score' hasn't been created yet.
  Try: Add `let score be a standard number with value ...` before this line.
```

Wrong number of arguments:
```
Oops! Line 8 has a problem.
  'add' needs 2 inputs, but you only gave it 1.
  Try: add of 3 and 5
```

Unclosed block:
```
Oops! Something isn't finished.
  A function or loop that started on line 4 was never closed.
  Try: Add `End.` after the last line of that block.
```

Rules:
- All errors begin with `Oops! Line N has a problem.` or `Oops! Something isn't finished.` for unclosed blocks
- Second line: plain description with no compiler jargon
- Third line: `Try:` with a concrete suggestion or corrected snippet
- Friendly messages go to stdout; standard error codes still go to stderr so editor tooling continues to work
- Non-beginner files use the existing error format unchanged

---

## What Stays the Same in Beginner Mode

- Variable declarations: `let x be a standard number with value 10.`
- All control flow: `If`, `otherwise`, `For each`, `While`, `stop`, `skip`
- Output: `output "Hello!".`
- All arithmetic statements: `Add`, `Subtract`, `Multiply`, `Divide`
- Classes: `Define a kind called ...` (unchanged — introduced later when kids are ready)
- Package imports: `use "numeric".`
- Array syntax: `arr[0]`, list literals `[1, 2, 3]`

---

## Example Beginner Program

```
use beginner.

To square x:
    let result be a standard number with value x.
    Multiply result by x.
    Give back result.
End.

To printSquares:
    For each i from 1 to 5,
        output square of i.
    End.
End.

Call printSquares.
```

---

## Out of Scope

- Tutorial system or guided exercises (future work)
- Type inference for variable declarations
- Visual/block-based output
- LSP or editor integration changes
