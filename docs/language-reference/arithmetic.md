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

To change a variable's value, use the compound assignment statements instead of `=`:

| Statement | Meaning |
|-----------|---------|
| `Add X to Y.` | `Y = Y + X` |
| `Subtract X from Y.` | `Y = Y - X` |
| `Multiply X to Y.` | `Y = Y * X` |
| `Divide X from Y.` | `Y = Y / X` |
