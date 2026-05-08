# eng-lish Language Reference

Welcome to the eng-lish language reference. Each page covers one part of the language with examples you can run.

## Core Language

| Feature | Summary |
|---------|---------|
| [Arithmetic](arithmetic.md) | `+`, `-`, `*`, `/` operators and compound assignment (`Add X to Y.`) |
| [Conditionals](conditionals.md) | `If condition then` / `otherwise` / `End.` — multi-statement branches |
| [Comments](comments.md) | `Note: ...` — line comments ignored by the compiler |
| Variables | `let name be a type with value ...` — store and reuse values |
| Functions | `To funcName with param returning type:` — write reusable code |
| Loops | `For each i from 0 to n,` and `While condition,` — repeat actions |
| Classes | `Define a kind called Name:` — group data and behavior together |
| Arrays | `list of standard number`, `list of decimal` — store lists of values |
| Strings | `"text"` with escape sequences `\"`, `\n`, `\t`, `\\` |
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
| [`neural`](neural-networks.md) | Neural networks: `initWeights`, `linearLayer`, `sigmoidActivation`, `mseError` |

## Examples

All example programs live in the `examples/` folder. Try running them with:

```
englishc examples/hello.eng
```
