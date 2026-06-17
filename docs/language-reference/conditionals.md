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
| `x is at least y` | x >= y |
| `x is at most y` | x <= y |
| `x is greater than or equal to y` | x >= y |
| `x is less than or equal to y` | x <= y |

### Comparing text

`is equal to` and `is not equal to` also work on text, which is how you check what someone typed:

```
let command be a text with value the result of readLine.
If command is equal to "quit" then
    output "Goodbye!".
End.
```

`is at least` and `is at most` are the short, friendly way to say "greater than or equal to" and "less than or equal to":

```
If age is at least 13 then
    output "You can sign up.".
End.

If lives is at most 0 then
    output "Game over.".
End.
```

## Inside loops

If blocks inside loops need their own `End.`:

```
For each i from 1 to 10,
    If the remainder of i divided by 2 is equal to 0 then
        skip.
    End.
    output i.
End.
```
