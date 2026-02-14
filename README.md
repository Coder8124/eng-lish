# eng-lish

**A programming language that reads like English.**

Write code the way you think — in plain, readable sentences.

```
let message be a text with value "Hello, World!".
output message.
```

---

## Why eng-lish?

Most programming languages prioritize brevity over clarity. eng-lish takes the opposite approach: **code should be readable by anyone**, even those who have never programmed before.

```
let age be a standard number with value 25.

If age is greater than 18 then
    output "You can vote!".
otherwise
    output "Too young to vote.".
```

No cryptic symbols. No memorizing operator precedence. Just English.

---

## Installation

### Prerequisites
- [Rust](https://rustup.rs/) (1.70+)
- LLVM 18+ (`brew install llvm` on macOS)

### Build from source
```bash
git clone https://github.com/yourusername/eng-lish.git
cd eng-lish
cargo install --path .
```

### Run a program
```bash
./engine examples/hello.eng
```

---

## Language Guide

### Variables

Declare variables with `let`:

```
let name be a text with value "Alice".
let age be a standard number with value 30.
let height be a decimal with value 5.9.
let isStudent be a boolean with value true.
```

**Types:**
| eng-lish | Description |
|----------|-------------|
| `standard number` | Integer (64-bit) |
| `decimal` | Float (64-bit) |
| `text` | String |
| `boolean` | True/False |

### Output

```
output "Hello!".
output age.
output name.
```

### Arithmetic

```
let x be a standard number with value 10.
let y be a standard number with value 3.

Add 5 to x.
Subtract 2 from y.
Multiply x by 2.
Divide x by 4.

output remainder x by y.
output quotient x by y.
```

### Comparisons

```
If x is greater than 5 then
    output "big".

If name is equal to "Alice" then
    output "Hi Alice!".

If x is not equal to 0 then
    output "not zero".
```

### Logic

```
let sunny be a boolean with value true.
let warm be a boolean with value false.

If sunny and warm then
    output "Perfect day!".

If sunny or warm then
    output "Nice enough.".

If not sunny then
    output "Bring an umbrella.".
```

### Loops

**For loops:**
```
For each i from 1 to 10,
    output i.
End.
```

**While loops:**
```
let count be a standard number with value 0.
While count is less than 5,
    output count.
    Add 1 to count.
```

**Control flow:**
```
For each i from 1 to 100,
    If i is equal to 5 then
        stop.
    If remainder i by 2 is equal to 0 then
        skip.
    output i.
End.
```

### Functions

```
To greet someone named name (a text),
    output "Hello, ".
    output name.
End.

Call greet with "World".
```

**With return values:**
```
To add together x (a standard number) and y (a standard number) returning a standard number,
    give back x plus y.
End.

let sum be a standard number with value the result of add with 3 and 4.
output sum.
```

### If-Else Chains

```
let score be a standard number with value 85.

If score is greater than 90 then
    output "A".
otherwise if score is greater than 80 then
    output "B".
otherwise if score is greater than 70 then
    output "C".
otherwise
    output "F".
```

---

## Standard Library

### Math Functions

| Function | Description | Example |
|----------|-------------|---------|
| `squareRoot` | Square root | `the result of squareRoot with 16` |
| `absoluteValue` | Absolute value | `the result of absoluteValue with -5` |
| `power` | Exponentiation | `the result of power with 2 and 10` |
| `floor` / `ceiling` / `round` | Rounding | `the result of floor with 3.7` |
| `sine` / `cosine` / `tangent` | Trigonometry | `the result of sine with 0` |
| `minimum` / `maximum` | Min/Max | `the result of minimum with 3 and 7` |
| `naturalLog` / `logarithm` | Logarithms | `the result of naturalLog with 2.718` |
| `random` | Random 0.0-1.0 | `the result of random` |
| `randomBetween` | Random in range | `the result of randomBetween with 1 and 100` |

### String Functions

| Function | Description | Example |
|----------|-------------|---------|
| `lengthOf` | String length | `the result of lengthOf with "hello"` |
| `combine` | Concatenate | `the result of combine with "Hello" and "World"` |
| `characterAt` | Get character | `the result of characterAt with "hello" and 0` |
| `uppercase` / `lowercase` | Case conversion | `the result of uppercase with "hello"` |
| `contains` | Substring check | `the result of contains with "hello" and "ell"` |

### I/O Functions

| Function | Description | Example |
|----------|-------------|---------|
| `readLine` | Read text input | `the result of readLine` |
| `readNumber` | Read number input | `the result of readNumber` |
| `readFile` | Read file contents | `the result of readFile with "data.txt"` |
| `writeFile` | Write to file | `the result of writeFile with "out.txt" and "Hello"` |

### Utility Functions

| Function | Description | Example |
|----------|-------------|---------|
| `textToNumber` | Parse int | `the result of textToNumber with "42"` |
| `textToDecimal` | Parse float | `the result of textToDecimal with "3.14"` |
| `numberToText` | Int to string | `the result of numberToText with 42` |
| `decimalToText` | Float to string | `the result of decimalToText with 3.14` |
| `sleep` | Pause (ms) | `Call sleep with 1000` |

### Array Functions

eng-lish includes NumPy-inspired array operations for working with lists of numbers.

**Array Literals and Indexing:**
```
let arr be a list of standard number with value [1, 2, 3, 4, 5].
output arr[0].
output arr[2].
```

| Function | Description | Example |
|----------|-------------|---------|
| `zeros` | Create array of zeros | `the result of zeros with 5` |
| `ones` | Create array of ones | `the result of ones with 5` |
| `range` | Create range array | `the result of range with 1 and 10` |
| `arrayLength` / `len` | Get array length | `the result of arrayLength with arr` |
| `sum` | Sum all elements | `the result of sum with arr` |
| `mean` / `average` | Calculate average | `the result of mean with arr` |
| `arrayMin` / `minOf` | Find minimum | `the result of arrayMin with arr` |
| `arrayMax` / `maxOf` | Find maximum | `the result of arrayMax with arr` |
| `append` / `push` | Add element | `the result of append with arr and 6` |
| `reverse` | Reverse array | `the result of reverse with arr` |

---

## Examples

### FizzBuzz

```
For each i from 1 to 100,
    If remainder i by 15 is equal to 0 then
        output "FizzBuzz".
    otherwise if remainder i by 3 is equal to 0 then
        output "Fizz".
    otherwise if remainder i by 5 is equal to 0 then
        output "Buzz".
    otherwise
        output i.
End.
```

### Guessing Game

```
let secret be a standard number with value the result of randomBetween with 1 and 100.
let guess be a standard number with value 0.

output "I'm thinking of a number between 1 and 100.".

While guess is not equal to secret,
    output "Your guess:".
    set guess to the result of readNumber.

    If guess is less than secret then
        output "Too low!".
    otherwise if guess is greater than secret then
        output "Too high!".
    otherwise
        output "Correct!".
```

### File I/O

```
let success be a boolean with value the result of writeFile with "notes.txt" and "Remember to buy milk.".

If success then
    output "Saved!".
    let content be a text with value the result of readFile with "notes.txt".
    output content.
otherwise
    output "Failed to save.".
```

---

## Error Messages

eng-lish provides friendly, helpful error messages:

```
Line 5: I don't know what 'foo' is. Did you forget to create it with 'let foo be...'?

Line 8: This expects a decimal, but you gave it a standard number. Try 5.0 instead of 5.

Line 12: 'add' needs 2 values, but you gave it 3.
```

---

## How It Works

eng-lish compiles to native machine code via LLVM:

```
┌──────────────┐     ┌────────┐     ┌──────────┐     ┌────────────┐
│  .eng file   │ ──► │ Lexer  │ ──► │  Parser  │ ──► │  Semantic  │
│              │     │        │     │          │     │  Analyzer  │
└──────────────┘     └────────┘     └──────────┘     └────────────┘
                                                            │
                                                            ▼
┌──────────────┐     ┌────────┐     ┌──────────┐     ┌────────────┐
│  Executable  │ ◄── │  Clang │ ◄── │ LLVM IR  │ ◄── │  Codegen   │
│              │     │        │     │          │     │            │
└──────────────┘     └────────┘     └──────────┘     └────────────┘
```

---

## Contributing

Contributions welcome! Areas of interest:

- More standard library functions
- Better error messages
- Language server (LSP) support
- Syntax highlighting for editors
- Documentation improvements

---

## License

 All Rights Reserved.

---

<p align="center">
  <i>Code should be readable. eng-lish makes it so.</i>
</p>
