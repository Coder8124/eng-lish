# Comments

Comments let you leave notes in your code. The compiler ignores them completely.

## Line comments

Start a line with `Note:` and the rest of the line is a comment.

```
Note: this whole line is a comment.
output "Hello!". Note: you can also put a comment after a statement.
```

## When to use comments

Comments are for explaining *why* something works a certain way, not *what* it does. Good variable and function names already explain what the code does.

Good:
```
Note: randomBetween seeds from time() on first call, so results differ each run.
let secret be a standard number with value the result of randomBetween with 1 and 100.
```

Not needed:
```
Note: add 1 to count.   ← the code already says this
Add 1 to count.
```
