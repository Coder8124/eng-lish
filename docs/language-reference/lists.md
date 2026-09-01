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

## Adding an item

`append` (or `push`) makes a new list with one more item added to the end. It works with any kind of list — numbers, decimals, text, even lists of things you built with `Define a kind`:

```
let scores be a list of standard number with value [10, 20, 30].
let more be a list of standard number with value the result of append with scores and 40.
output more[3].   Note: 40
```

```
let names be a list of text with value ["Ana", "Bo"].
let names2 be a list of text with value the result of append with names and "Cy".
output names2[2].   Note: Cy
```

`append` doesn't change the original list — it hands back a brand new one, so `scores` above still only has 3 items.

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
