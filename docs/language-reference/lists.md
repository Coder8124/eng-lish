# Lists

A list holds many values in order. You make one with square brackets:

```
let scores be a list of standard number with value [10, 20, 30, 40].
```

## Reading an item

Items are numbered starting at `0`. Use square brackets to read one:

```
output scores[0].   Note: 10
output scores[2].   Note: 30
```

The number inside the brackets can be a variable too:

```
let i be a standard number with value 1.
output scores[i].   Note: 20
```

## Changing an item

Use `Set` with the brackets to change one item in place:

```
Set scores[1] to 99.
output scores[1].   Note: 99
```

The new value can be any expression, including the list itself:

```
Set scores[3] to scores[0] + scores[1].
```

## How long is it?

`arrayLength` tells you how many items a list has:

```
output the result of arrayLength with scores.   Note: 4
```

## Making a list of a certain size

`range` builds a list of whole numbers from a start up to (but not including) an end. It is a handy way to make a list of the right size that you then fill in:

```
let grid be a list of standard number with value the result of range with 0 and 64.
Set grid[0] to 5.
```

This is exactly how the games in `examples/games/` build their maps.
