# eng-lish

**A compiled programming language that reads like English.**

Write code the way you think — in plain, readable sentences. No symbols to memorize, no syntax to look up.

```
let message be a text with value "Hello, World!".
output message.
```

---

## Why eng-lish?

Most languages prioritize brevity over clarity. eng-lish takes the opposite approach: **code should be readable by anyone**, even those who have never programmed before.

```
let age be a standard number with value 25.

If age is greater than 18 then
    output "You can vote!".
otherwise
    output "Too young to vote.".
```

eng-lish compiles to native machine code via LLVM — so it's readable *and* fast.

---

## Installation

### Prerequisites
- [Rust](https://rustup.rs/) (1.70+)
- LLVM 18+ (`brew install llvm` on macOS)
- Clang (for linking)

### Build from source
```bash
git clone https://github.com/Coder8124/eng-lish.git
cd eng-lish
cargo install --path .
```

### Compile and run a program
```bash
englishc examples/hello.eng
./hello
```

### Print LLVM IR (for debugging)
```bash
englishc examples/hello.eng --ir
```

---

## Language Guide

### Variables

```
let name be a text with value "Alice".
let age be a standard number with value 30.
let height be a decimal with value 5.9.
let isStudent be a boolean with value true.
```

**Types:**

| eng-lish | Description |
|---|---|
| `standard number` | 64-bit integer |
| `decimal` | 64-bit float |
| `text` | String |
| `boolean` | `true` / `false` |
| `list of <type>` | Typed array |

### Output

```
output "Hello!".
output age.
```

### Arithmetic

**Compound assignment** (modifies in place):
```
let x be a standard number with value 10.

Add 5 to x.
Subtract 2 from x.
Multiply x by 3.
Divide x by 4.
```

**Expressions:**
```
output remainder x by 3.
output quotient x by 3.
```

### Comparisons and Logic

```
If x is greater than 5 then
    output "big".

If x is less than 5 then
    output "small".

If x is equal to 5 then
    output "just right".

If x is not equal to 0 then
    output "not zero".
```

```
If sunny and warm then
    output "Perfect day!".

If sunny or warm then
    output "Nice enough.".

If not sunny then
    output "Bring an umbrella.".
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

### Loops

**For loop:**
```
For each i from 1 to 10,
    output i.
End.
```

**While loop:**
```
let count be a standard number with value 0.
While count is less than 5,
    output count.
    Add 1 to count.
End.
```

**Break and continue:**
```
For each i from 1 to 100,
    If remainder i by 2 is equal to 0 then
        skip.
    If i is greater than 9 then
        stop.
    output i.
End.
```

### Functions

```
To double with a standard number x returning a standard number:
    let result be a standard number with value x.
    Multiply result by 2.
    Give back result.
End.

let answer be a standard number with value the result of double with 21.
output answer.
```

Functions with no return value use `returning nothing`:
```
To greet with a text name returning nothing:
    output "Hello, ".
    output name.
End.

Call greet with "World".
```

### Classes

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

let c be a Counter created with 0.
Ask c to increment.
Ask c to increment.
output the result of asking c to getValue.
```

### Lists

```
let numbers be a list of standard number with value [10, 20, 30, 40, 50].

output numbers[0].
output numbers[2].

let size be a standard number with value the result of arrayLength with numbers.
let extended be a list of standard number with value the result of append with numbers and 60.
let flipped be a list of standard number with value the result of reverse with numbers.
```

---

## Standard Library

### Math

| Function | Description |
|---|---|
| `squareRoot` | Square root |
| `absoluteValue` | Absolute value |
| `power` | Exponentiation |
| `floor` / `ceiling` / `round` | Rounding |
| `sine` / `cosine` / `tangent` | Trig |
| `arcSine` / `arcCosine` / `arcTangent` | Inverse trig |
| `naturalLog` / `logarithm` / `exponential` | Logarithms |
| `minimum` / `maximum` | Min/Max of two values |
| `random` | Random decimal 0.0–1.0 |
| `randomBetween` | Random integer in range |

### Text

| Function | Description |
|---|---|
| `lengthOf` | String length |
| `combine` | Concatenate two strings |
| `characterAt` | Character at index |
| `uppercase` / `lowercase` | Case conversion |
| `contains` | Substring check |

### Input / Output

| Function | Description |
|---|---|
| `readLine` | Read a line of text from stdin |
| `readNumber` | Read an integer from stdin |
| `readFile` | Read entire file as text |
| `writeFile` | Write text to a file |

### Conversion

| Function | Description |
|---|---|
| `textToNumber` | Parse integer from text |
| `textToDecimal` | Parse decimal from text |
| `numberToText` | Integer to text |
| `decimalToText` | Decimal to text |
| `sleep` | Pause for N milliseconds |

### Arrays

| Function | Description |
|---|---|
| `zeros` / `ones` | Create array of zeros or ones |
| `range` | Create integer range array |
| `arrayLength` | Length of an array |
| `sum` / `mean` | Aggregate an array |
| `arrayMin` / `arrayMax` | Min/max of an array |
| `append` / `reverse` | Manipulate arrays |
| `standardDeviation` / `variance` | Statistics |
| `correlation` | Pearson correlation |
| `fitLine` / `predictLinear` | Linear regression |

---

## Package Manager

eng-lish has a built-in package manager. Packages are `.eng` files — just functions and classes your program can `use`.

### Using a package

```
use "numeric".
use "geometry".

output the result of factorial with 10.
output the result of circleArea with 5.0.
```

### Installing a package

```bash
englishc install https://github.com/user/eng-lish-somepackage
```

This clones the repo into `~/.eng-lish/packages/`. From then on, `use "somepackage".` resolves automatically.

### Package resolution order

1. `<same directory as source file>/<name>.eng`
2. `./packages/<name>/<name>.eng` (project-local packages)
3. `~/.eng-lish/packages/<name>/<name>.eng` (installed packages)
4. `~/.eng-lish/packages/<name>/main.eng`

### Bundled packages

The repo ships three packages under `packages/`:

**`numeric`** — inspired by `<numeric>` and `<cstdlib>`
```
use "numeric".

output the result of factorial with 10.
output the result of fibonacci with 15.
output the result of gcd with 48 and 18.
output the result of lcm with 4 and 6.
output the result of isPrime with 17.
output the result of sumUpTo with 100.
```

**`algorithm`** — inspired by `<algorithm>`
```
use "algorithm".

let data be a list of standard number with value [3, 7, 1, 9, 4].

output the result of clamp with 15 and 0 and 10.
output the result of linearSearch with data and 9.
output the result of countOccurrences with data and 3.
output the result of maxIndex with data.
```

**`geometry`** — inspired by `<cmath>` + computational geometry
```
use "geometry".

output the result of distance with 0.0 and 0.0 and 3.0 and 4.0.
output the result of hypotenuse with 3.0 and 4.0.
output the result of circleArea with 5.0.
output the result of triangleArea with 6.0 and 4.0.
output the result of degreesToRadians with 90.0.

let p be a Point created with 0.0 and 0.0.
output the result of asking p to distanceTo with 3.0 and 4.0.
```

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

### Guessing game

```
let secret be a standard number with value the result of randomBetween with 1 and 100.
let guess be a standard number with value 0.

output "I am thinking of a number between 1 and 100.".

While guess is not equal to secret,
    output "Your guess:".
    Set guess to the result of readNumber.

    If guess is less than secret then
        output "Too low!".
    otherwise if guess is greater than secret then
        output "Too high!".
    otherwise
        output "Correct!".
End.
```

### File I/O

```
let ok be a boolean with value the result of writeFile with "notes.txt" and "Buy milk.".

If ok then
    output "Saved!".
    let contents be a text with value the result of readFile with "notes.txt".
    output contents.
otherwise
    output "Could not save file.".
```

---

## How It Works

eng-lish compiles to native machine code via LLVM:

```
.eng source
    │
    ▼
  Lexer  ──►  Parser  ──►  Semantic Analyzer
                                  │
                                  ▼
 Executable  ◄──  Clang  ◄──  Codegen (LLVM IR)
```

---

## Contributing

Contributions welcome. Areas of interest:

- More packages
- Better error messages
- Language server (LSP) support
- Syntax highlighting for editors

---

## License

All Rights Reserved.

---

*Code should be readable. eng-lish makes it so.*
