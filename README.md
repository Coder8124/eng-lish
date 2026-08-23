# eng-lish

**A compiled programming language that reads like English.**

Write code the way you think — in plain, readable sentences. No symbols to memorize, no syntax to look up. eng-lish compiles to native machine code via LLVM, so it's readable *and* fast.

```
let message be a text with value "Hello, World!".
output message.
```

---

## Installation

### Prerequisites
- [Rust](https://rustup.rs/) (1.85+, for edition 2024)
- LLVM 21.1 exactly — `brew install llvm@21` on macOS, then set `LLVM_SYS_211_PREFIX`
- Clang (for linking)

LLVM 22 and LLVM 20 will not work: `inkwell` is pinned to one LLVM major version.
See [CONTRIBUTING.md](CONTRIBUTING.md) for the full setup.

### Build from source
```bash
git clone https://github.com/Coder8124/eng-lish.git
cd eng-lish
cargo install --path .
```

### Compile and run
```bash
englishc examples/hello.eng
./examples/binaries/hello
```

### Print LLVM IR (for debugging)
```bash
englishc examples/hello.eng --ir
```

---

## Quick Tour

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

### Arithmetic

**Infix operators** (use in any expression):
```
let area be a standard number with value width * height.
let average be a decimal with value total / count.
let distance be a standard number with value end - start.
let total be a standard number with value price + tax.
```

**Compound assignment** (modifies a variable in place):
```
let x be a standard number with value 10.

Add 5 to x.        Note: x = x + 5
Subtract 2 from x. Note: x = x - 2
Multiply x by 3.   Note: x = x * 3
Divide x by 4.     Note: x = x / 4
```

**Remainder and quotient:**
```
output the remainder of x divided by 3.
output the quotient of x divided by 3.
```

### Comments

```
Note: this is a comment — it is ignored by the compiler.
output "Hello!". Note: inline comment after a statement.
```

### Output and Input

```
output "Enter your name:".
let name be a text with value the result of readLine.
output "Hello, ".
output name.
```

### Control Flow

**If/otherwise** — requires `End.` to close:
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
End.
```

Multiple statements per branch are supported:
```
If score is greater than 90 then
    output "Great job!".
    output "You got an A.".
otherwise
    output "Keep practicing.".
End.
```

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
    If the remainder of i divided by 2 is equal to 0 then
        skip.
    End.
    If i is greater than 9 then
        stop.
    End.
    output i.
End.
```

### Functions

```
To double with a standard number x returning a standard number:
    let doubled be a standard number with value x * 2.
    Give back doubled.
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

### File I/O

```
let ok be a boolean with value the result of writeFile with "notes.txt" and "Buy milk.".

If ok then
    output "Saved!".
    let contents be a text with value the result of readFile with "notes.txt".
    output contents.
otherwise
    output "Could not save file.".
End.
```

---

## Beginner Mode

Add `use beginner.` at the top of your program to enable a friendlier experience for new programmers:

- Function calls use a simpler `name of arg` syntax instead of `the result of name with arg`
- Error messages say `Oops! Line 5 has a problem...` instead of compiler jargon
- Type inference relaxes in several places

```
use beginner.

output "What is your name?".
let name be a text with value the result of readLine.
output combine of "Hello, " and name.
```

---

## Data Science

eng-lish has built-in support for statistics, machine learning, and charting — without leaving the language.

### Plotting

```
use "numeric".

let data be a list of decimal with value [2.0, 4.0, 8.0, 16.0, 32.0].
plot data as a line chart titled "Growth" to "chart.html".
```

Supported chart types: `line chart`, `bar chart`, `scatter plot`, `histogram`.

### Neural Networks

```
use "neural".

let w1 be a list of decimal with value the result of initWeights with 2 and 4.
let b1 be a list of decimal with value the result of initBiases with 4.
let input be a list of decimal with value [0.5, 0.8].

let hidden be a list of decimal with value the result of linearLayer with w1 and b1 and input and 4 and 2.
let hiddenOut be a list of decimal with value the result of sigmoidActivation with hidden.
```

See `packages/neural/README.md` for the full API.

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
| `naturalLog` / `logarithm` / `exponential` | Logarithms |
| `minimum` / `maximum` | Min/max of two values |
| `random` | Random decimal 0.0–1.0 |
| `randomBetween` | Random integer in a range |

### Text

| Function | Description |
|---|---|
| `lengthOf` | String length |
| `combine` | Concatenate two strings |
| `characterAt` | Character at index |
| `uppercase` / `lowercase` | Case conversion |
| `contains` | Substring check |

Escape sequences in strings: `\"`, `\n`, `\t`, `\\`.

### Input / Output

| Function | Description |
|---|---|
| `readLine` | Read a line of text from stdin |
| `readNumber` | Read an integer from stdin |
| `readFile` | Read entire file as text |
| `writeFile` | Write text to a file |
| `sleep` | Pause for N milliseconds |

### Conversion

| Function | Description |
|---|---|
| `textToNumber` | Parse integer from text |
| `textToDecimal` | Parse decimal from text |
| `numberToText` | Integer to text |
| `decimalToText` | Decimal to text |

### Arrays

| Function | Description |
|---|---|
| `zeros` / `ones` | Create float array of zeros or ones |
| `range` | Create integer range array |
| `arrayLength` / `vectorLength` | Length of an int or float array |
| `sum` / `mean` | Aggregate a float array |
| `arrayMin` / `arrayMax` | Min/max of a float array |
| `append` / `reverse` | Manipulate arrays |
| `standardDeviation` / `variance` | Statistics |
| `correlation` | Pearson correlation |
| `fitLine` / `predictLinear` | Linear regression |

---

## Packages

eng-lish has a built-in package manager. Packages are `.eng` files — just functions and classes your program can `use`.

### Using a package

```
use "numeric".

output the result of factorial with 10.
output the result of fibonacci with 15.
output the result of isPrime with 17.
```

### Installing a package from GitHub

```bash
englishc install https://github.com/user/eng-lish-somepackage
```

Clones the repo into `~/.eng-lish/packages/`. After that, `use "somepackage".` resolves automatically.

### Bundled packages

| Package | What it provides |
|---|---|
| `numeric` | `factorial`, `fibonacci`, `gcd`, `lcm`, `isPrime`, `sumUpTo` |
| `algorithm` | `clamp`, `linearSearch`, `countOccurrences`, `maxIndex`, `minIndex` |
| `geometry` | `distance`, `hypotenuse`, `circleArea`, `triangleArea`, `degreesToRadians`, `Point` class |
| `math` | `sign`, `absoluteInt`, `isEven`, `isOdd`, `digitCount`, `intPow` |
| `strings` | `repeat`, `padLeft`, `padRight`, `isEmpty` |
| `neural` | `initWeights`, `initBiases`, `linearLayer`, `sigmoidActivation`, `reluActivation`, `softmaxActivation`, `mseError`, `updateWeights` |

---

## Examples

### FizzBuzz

```
For each i from 1 to 100,
    If the remainder of i divided by 15 is equal to 0 then
        output "FizzBuzz".
    otherwise if the remainder of i divided by 3 is equal to 0 then
        output "Fizz".
    otherwise if the remainder of i divided by 5 is equal to 0 then
        output "Buzz".
    otherwise
        output i.
    End.
End.
```

### Guessing Game

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
End.
```

---

## How It Works

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

MIT — see [LICENSE](LICENSE). You are free to use, modify, and distribute eng-lish, including in your own projects.

---

*Code should be readable. eng-lish makes it so.*
