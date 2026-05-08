# Conditionals

Use `If` to run code only when something is true. Every `If` block must end with `End.`

## Basic If

```
If score is greater than 90 then
    output "You got an A!".
End.
```

## If/otherwise (else)

```
If score is greater than 60 then
    output "You passed.".
otherwise
    output "You did not pass.".
End.
```

## If/otherwise if chains

```
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

## Multiple statements per branch

Each branch can contain as many statements as you need:

```
If score is greater than 90 then
    output "Great job!".
    output "You got an A.".
    Add 10 to bonus.
otherwise
    output "Keep practicing.".
End.
```

## Comparisons

| Expression | Meaning |
|---|---|
| `x is greater than y` | x > y |
| `x is less than y` | x < y |
| `x is equal to y` | x == y |
| `x is not equal to y` | x != y |

## Inside loops

If blocks inside loops need their own `End.`:

```
For each i from 1 to 10,
    If remainder i by 2 is equal to 0 then
        skip.
    End.
    output i.
End.
```
