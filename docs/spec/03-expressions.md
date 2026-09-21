# 3. Expressions

An expression produces a value. Implemented in `src/parser.rs`, as recursive descent.

## 3.1 Precedence

From loosest to tightest. Everything on one row binds equally and groups left to right unless stated.

| | Level | Written | Groups |
|---|---|---|---|
| 1 | Or | `or` | left |
| 2 | And | `and` | left |
| 3 | Comparison | `is …` phrases | left |
| 4 | Addition | `+` `-` | left |
| 5 | Multiplication | `*` `/` | left |
| 6 | Remainder | `remainder … by …`, `quotient … by …` | see 3.4 |
| 7 | Prefix | `not`, `negative` | right |
| 8 | Indexing | `value[index]` | left |
| 9 | Primary | literals, names, `( … )`, `the …` forms | |

So `a + b * c` is `a + (b * c)`, and `x is at least 1 and y is at least 1` is `(x is at least 1) and (y is at least 1)`.

**There are no word forms for arithmetic inside an expression.** `Add`, `Subtract`, `Multiply`, and `Divide` are whole sentences (see [4.4](04-statements.md)); inside an expression the only arithmetic operators are the symbols `+ - * /`.

## 3.2 Comparisons

```ebnf
comparison = additive { "is" [ "not" ] comparison-operator additive } ;
```

Every accepted phrase, and what it means:

| Phrase | Means | With `not` | Means |
|---|---|---|---|
| `is equal to` | `=` | `is not equal to` | `≠` |
| `is equal` | `=` | `is not equal` | `≠` |
| `is same` / `is same to` | `=` | `is not same` | `≠` |
| `is greater than` | `>` | `is not greater than` | `≤` |
| `is less than` | `<` | `is not less than` | `≥` |
| `is greater than or equal to` | `≥` | `is not greater than or equal to` | `<` |
| `is less than or equal to` | `≤` | `is not less than or equal to` | `>` |
| `is at least` | `≥` | `is not at least` | `<` |
| `is at most` | `≤` | `is not at most` | `>` |

The trailing `to` is optional in `is equal to` and in `is greater than or equal to` and its relatives. The word `than` is required after `greater` and `less`.

`is at least` and `is at most` are the recommended forms — they are shorter and read better than the four-word versions.

### Phrases that do not work

**`is the same as` is not valid eng-lish**, despite reading like it should be. It fails to parse. Write `is same to` or, better, `is equal to`.

Comparisons may be chained by the grammar, but the type checker will reject the result, because comparing a boolean against a number is not allowed.

## 3.3 Logic

`and`, `or`, and `not` take booleans and produce a boolean. `negative` negates a number.

Be aware that `and` is also the separator between arguments in a function call (3.6). Inside an argument list, a bare `and` is read as a separator, so a logical `and` in an argument **must** be parenthesised:

```
Call check with (isReady and isWilling).
```

## 3.4 Remainder and quotient

There are two ways to write each, and they behave identically:

```ebnf
the-division = "the" ( "remainder" | "quotient" ) "of" unary [ "divided" ] "by" unary ;
bare-division = ( "remainder" | "quotient" ) unary "by" unary ;
```

```
output the remainder of 10 divided by 3.     Note: prints 1 -- preferred
output the remainder of 10 by 3.             Note: same thing
output remainder 10 by 3.                    Note: same thing, terse
```

The long `the remainder of X divided by Y` form is the one to use in new code.

Both operands bind at the prefix level, which is tighter than `*` and `/`. That means `remainder 10 by 3 * 2` groups as `(remainder 10 by 3) * 2`, which is probably not what you meant. Parenthesise anything more complicated than a name or a literal.

## 3.5 The `the …` forms

Several expressions start with `the`. It is not a general-purpose word — it introduces exactly these forms:

```ebnf
the-form = "the result of" identifier [ "with" arguments ]
         | "the result of asking" identifier "to" identifier [ "with" arguments ]
         | "the" identifier "of" identifier
         | the-division ;
```

| Form | What it does |
|---|---|
| `the result of f` | Calls the function `f` with no arguments |
| `the result of f with x and y` | Calls `f` with two arguments |
| `the result of asking obj to method` | Calls a method and uses its answer |
| `the width of box` | Reads a property |
| `the gradient of w` / `the value of w` | Reads a `decimal` out of the watched decimal `w` |
| `the shape of t` / `the value of t` | A tensor's sizes as a `list of standard number`, or its only number as a `decimal` |
| `the sum of t` / `the mean of t` / `the transpose of t` | A tensor worked out from the tensor `t` |

Property access is deliberately shallow: both the property and the object **must** be plain names. `the name of the owner of car` does not parse. **There is no dot syntax anywhere in eng-lish** — `box.width` is not valid.

### Tensor functions

| Call | Inputs | Result |
|---|---|---|
| `zeroTensor`, `oneTensor`, `randomTensor` | One or more whole-number sizes | A tensor of that shape filled with 0, 1, or standard-normal random numbers from a fixed seed |
| `reshape` | A tensor, then one or more sizes | The same numbers in a new shape with the same count |
| `matmul` (also `matrixMultiply`) | Two tensors with 1 or 2 dimensions | The matrix product; the first's last size **must** equal the second's first size |
| `transpose` | A tensor | The dimensions reversed |
| `sumAlong`, `meanAlong` | A tensor and a direction | The dimension at that direction added up (or averaged) and removed |
| `sigmoid`, `relu`, `tanh`, `exponential`, `logarithm`, `softmax` | A tensor | Applied to each number; `softmax` works along the last dimension |
| `power` | A tensor and a number | Each number raised to that power |

`sigmoid`, `relu`, `softmax`, `power` and the other shared names mean the tensor versions only when their first input is a `tensor`. With a list they are the list built-ins. Indexing a tensor, `t[i]`, removes its first dimension.

## 3.6 Calls

```ebnf
arguments = expression { "and" expression } ;
```

Arguments are separated by the word `and`, not by commas. Each argument is parsed at the comparison level, which is what makes the bare-`and` restriction in 3.3 necessary.

A function call used as an expression **must** be written as `the result of …`. A call written as a whole sentence uses `Call` instead, and discards the answer — see [4.8](04-statements.md).

## 3.7 Lists and indexing

```ebnf
list-literal = "[" [ expression { "," expression } ] "]" ;
index = postfix "[" expression "]" ;
```

List literals use commas, unlike argument lists. Every element **must** have the same type. The empty list `[]` is a `list of standard number`.

```
Let scores be a list with value [10, 20, 30].
output scores[0].          Note: prints 10 -- counting starts at zero
```

Indexing works on three things: a list (indexed by a whole number), a dictionary (indexed by its key type), and text (indexed by a whole number, giving one character back as text). Indexing chains, so `grid[row][column]` works.

**There is no dictionary literal.** Build one a key at a time:

```
Let ages be a new dictionary.
Set ages["Alice"] to 12.
```

## 3.8 Objects

```ebnf
object-creation = identifier "created with" arguments ;
```

```
Let counter be a Counter created with 0.
```

There is no `new` keyword. The word `new` appears only in the dictionary declaration form `a new dictionary`, where it is an ordinary word the parser looks for by name.
