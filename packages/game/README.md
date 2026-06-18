# game package

Little helpers for drawing text-based games in the terminal: boxes, rows, and centered titles. Use it with:

```
use "game".
```

## Functions

### `printBorder with <width>`

Prints a top or bottom border made of `#` corners and `-` in between.

```
Call printBorder with 10.
```

Output:

```
#----------#
```

### `printRow with <text>`

Prints one row of content wrapped in side walls: `| content |`.

```
Call printRow with "hello".
```

Output:

```
| hello |
```

### `centerText with <text> and <width>`

Returns the text padded with spaces so it sits in the middle of the given width. Handy for titles. If the text is already as wide as `width`, it is returned unchanged.

```
Call printRow with the result of centerText with "TITLE" and 11.
```

Output:

```
|    TITLE    |
```

## A tiny example

```
use "game".

Call printBorder with 20.
Call printRow with the result of centerText with "BLOCK WORLD" and 18.
Call printBorder with 20.
```

Output:

```
#--------------------#
|    BLOCK WORLD     |
#--------------------#
```

## Builtins it builds on

The `game` package is a thin wrapper around two built-in helpers you can also use directly:

| Builtin | What it does |
|---------|--------------|
| `repeatText with <text> and <count>` | Returns the text repeated `count` times. `repeatText with "-" and 5` → `"-----"`. |
| `clearScreen` | Clears the terminal and moves the cursor to the top. Call it with `Call clearScreen.` |

## Games to try

Two complete games live in `examples/games/`:

- `block_world.eng` — explore an 8×8 world, chop trees, and mine stone.
- `kart_race.eng` — a turn-based race against three CPU karts, with items.

Both have a **MOD ZONE** at the top: change the numbers to change the game. Each one also has a clearly marked spot for you to write your own code (a crafting recipe in Block World, the Blue Shell item in Kart Race).
