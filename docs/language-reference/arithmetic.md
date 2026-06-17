# Arithmetic Operators

eng-lish supports the standard math operators using symbols. You can use them directly in expressions anywhere a value is expected.

## Operators

| Operator | Meaning | Example |
|----------|---------|---------|
| `+` | Add | `x + y` |
| `-` | Subtract | `total - 1` |
| `*` | Multiply | `width * height` |
| `/` | Divide | `sum / count` |

## Examples

```
let area be a standard number with value width * height.
let average be a decimal with value total / count.
let distance be a decimal with value endX - startX.
```

You can chain operators and they follow standard order of operations (multiply and divide before add and subtract):

```
let result be a decimal with value base + rate * time.
```

## Using operators in loop ranges

```
For each idx from 0 to size - 1,
    output arr[idx].
End.
```

## Using operators with array indexing

```
let product be a decimal with value vec1[idx] * vec2[idx].
```

## Compound assignment

To change a variable's value, use the compound assignment statements. They read like plain English: you say what you are changing and by how much.

| Statement | Meaning |
|-----------|---------|
| `Add X to Y.` | `Y = Y + X` |
| `Subtract X from Y.` | `Y = Y - X` |
| `Multiply Y by X.` | `Y = Y * X` |
| `Divide Y by X.` | `Y = Y / X` |

Notice the word order matches how you would say it out loud: you *add 5 to the total*, but you *multiply the total by 2*.

```
let total be a standard number with value 100.
Add 20 to total.        Note: total is now 120
Subtract 5 from total.  Note: total is now 115
Multiply total by 2.    Note: total is now 230
Divide total by 10.     Note: total is now 23
```

The older forms `Multiply X to Y.` and `Divide X from Y.` still work, but `Multiply Y by X.` and `Divide Y by X.` are the recommended, clearer way.

## Remainder and quotient

When you divide whole numbers you sometimes want the leftover (the remainder) or the whole-number result (the quotient).

| Expression | Meaning |
|------------|---------|
| `the remainder of X divided by Y` | what is left over after dividing `X` by `Y` |
| `the quotient of X divided by Y` | how many whole times `Y` goes into `X` |

```
let leftover be a standard number with value the remainder of 17 divided by 5.   Note: 2
let times be a standard number with value the quotient of 17 divided by 5.       Note: 3

If the remainder of n divided by 2 is equal to 0 then
    output "n is even".
End.
```
