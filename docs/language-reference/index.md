# eng-lish Language Reference

Welcome to the eng-lish language reference. Each page covers one part of the language with examples you can run.

Looking for the exact rules instead? The [language specification](../spec/index.md) defines precisely what the compiler accepts, keyword by keyword. These pages teach; the specification defines.

## Core Language

| Feature | Summary |
|---------|---------|
| [Arithmetic](arithmetic.md) | `+`, `-`, `*`, `/`, compound assignment (`Multiply total by 2.`), and `the remainder/quotient of X divided by Y` |
| [Conditionals](conditionals.md) | `If condition then` / `otherwise` / `End.`, comparisons like `is at least` / `is at most` |
| [Comments](comments.md) | `Note: ...` — line comments ignored by the compiler |
| Variables | `let name be a type with value ...` — store and reuse values |
| Functions | `To funcName with param returning type:` — write reusable code |
| Loops | `For each i from 0 to n,` and `While condition,` — repeat actions |
| Classes | `Define a kind called Name:` — group data and behavior together |
| [Lists](lists.md) | `[1, 2, 3]`, reading `scores[0]`, and changing items with `Set scores[0] to ...` |
| [Dictionaries](dictionaries.md) | `Let ages be a new dictionary.`, `Set ages["Alice"] to 12.`, `Remove "Alice" from ages.`, plus `hasKey` / `keysOf` / `sizeOf` |
| Strings | `"text"` with escape sequences `\"`, `\n`, `\t`, `\\`; compare with `is equal to` |
| File I/O | `readFile` / `writeFile` — read and write files |

## Beginner Mode

Add `use beginner.` at the top of your program to unlock kid-friendly error messages and simplified syntax. Great for first-time programmers.

## Packages

Packages add extra functions to your program. Use them with `use "packageName".` at the top.

| Package | Summary |
|---------|---------|
| `numeric` | Math functions: `mean`, `variance`, `standardDeviation`, `correlation`, `fitLine` |
| `algorithm` | Sorting and searching: `sort`, `binarySearch` |
| `geometry` | Shapes and distances: `circleArea`, `distance`, `pythagorean` |
| `strings` | String utilities: `length`, `toUpperCase`, `contains` |
| `math` | Math constants and functions: `pi`, `sqrt`, `abs`, `power` |
| [`game`](game-package.md) | Terminal game helpers: `printBorder`, `printRow`, `centerText` |
| [`neural`](neural-networks.md) | Neural networks: `initWeights`, `linearLayer`, `sigmoidActivation`, `mseError` |

## Calculators

eng-lish programs can run on TI-83/84 graphing calculators. See [TI-BASIC output](ti-basic.md) — compile with `--ti-basic` and send the result to your calculator.

## Examples

All example programs live in the `examples/` folder. Try running them with:

```
englishc examples/hello.eng
```
