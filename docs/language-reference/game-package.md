# The game package

The `game` package gives you simple tools for drawing games in the terminal. It is built for making little text games like the ones in `examples/games/`.

Turn it on at the top of your program:

```
use "game".
```

## What you can do

### Draw a border

`printBorder` draws a line of `#` corners with `-` between them. You tell it how wide.

```
Call printBorder with 12.
```

```
#------------#
```

### Draw a row

`printRow` prints text inside two walls, like one row of a box.

```
Call printRow with "player @".
```

```
| player @ |
```

### Center a title

`centerText` adds spaces around text so it sits in the middle. Give it the text and how wide the space is. It hands back the centered text, so you usually print it with `printRow`.

```
Call printRow with the result of centerText with "GAME OVER" and 20.
```

```
|      GAME OVER      |
```

## Two built-in helpers

The package uses two built-in functions that you can also call on their own:

### Repeat some text

`repeatText` glues a piece of text to itself a number of times.

```
let bar be a text with value the result of repeatText with "=" and 10.
output bar.
```

```
==========
```

### Clear the screen

`clearScreen` wipes the terminal and puts the cursor back at the top. It is great for redrawing a game each turn so it looks like it is animating.

```
Call clearScreen.
```

## Put it together

```
use "game".

Call clearScreen.
Call printBorder with 20.
Call printRow with the result of centerText with "MY GAME" and 18.
Call printBorder with 20.
```

## Ready-made games

Open these in `examples/games/` and run them:

- **`block_world.eng`** — walk around an 8×8 world with `north`, `south`, `east`, `west`. `mine` to chop trees and dig stone.
- **`kart_race.eng`** — race three computer karts. `gas` to drive, `item` to use what you are holding.

Both files start with a **MOD ZONE**: a block of numbers you can change to make the game your own (bigger maps, longer tracks, better luck with items). Each game also leaves one spot for you to write your own code — a crafting recipe in Block World, and the Blue Shell item in Kart Race.
