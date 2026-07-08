# Running Your Programs on a TI Calculator

eng-lish can turn your program into TI-BASIC — the language TI-83 and TI-84 graphing calculators understand. Write your program once, run it on your computer, then take it to math class.

## How to use it

```
englishc examples/quadratic.eng --ti-basic
```

This creates `quadratic.8xp.txt` next to your program and prints the TI-BASIC code. To get it onto your calculator:

1. Open **TI Connect CE** (free from TI) and start a new program in the Program Editor, or use the SourceCoder website.
2. Paste in the code.
3. Send it to your calculator over the USB cable.

## What works on a calculator

| eng-lish | TI-BASIC |
|----------|----------|
| `Let x be a standard number with value 5.` | `5→A` |
| `output "HELLO".` | `Disp "HELLO"` |
| `the result of readNumber` | `Input A` |
| `If` / `otherwise` / `End.` | `If` / `Then` / `Else` / `End` |
| `While` and `For each` loops | `While` / `For(` |
| Lists like `[1, 2, 3]` and `nums[i]` | `{1,2,3}→L1` and `L1(A+1)` |
| Math: `squareRoot`, `abs`, `round`, `sin`, `cos`, `min`, `max`, `power` | `√(`, `abs(`, `round(`, ... |
| `randomBetween` | `randInt(` |

Number variables become the letters `A` through `Z`, text becomes `Str0`–`Str9`, and lists become `L1`–`L6` — that's all the calculator has, so keep programs small.

## What doesn't work (yet)

If your program uses any of these, the compiler tells you exactly which line to fix:

- Functions (`To ... :`) and packages
- Kinds (classes)
- `stop.` and `skip.` inside loops
- `plot`, file reading and writing
- Dictionaries

## Try it

`examples/quadratic.eng` solves ax² + bx + c = 0 and runs on both your computer and your calculator — the exact same file.
