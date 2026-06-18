# Game Library Design

**Date:** 2026-05-04
**Branch:** lib/game

## Overview

Add a `game` package and two playable terminal games to eng-lish. Games follow the Code.org model: labeled constants beginners can tweak, plus intentional stub functions they fill in to unlock features.

---

## 1. New stdlib builtins

Two builtins added to `src/stdlib.rs` and implemented in `src/codegen.rs`:

| Name | Signature | Description |
|------|-----------|-------------|
| `clearScreen` | `() → nothing` | Outputs ANSI escape `\033[2J\033[H` to clear the terminal |
| `repeatText` | `(text, Int) → text` | Returns the string repeated N times (e.g. `"-"` × 10 → `"----------"`) |

---

## 2. `packages/game/game.eng`

Thin wrapper package. Exports:

| Function | Signature | Description |
|----------|-----------|-------------|
| `printBorder` | `(Int) → nothing` | Prints `#` + repeated `-` + `#` for given width |
| `printRow` | `(text) → nothing` | Wraps content: `\| content \|` |
| `centerText` | `(text, Int) → text` | Pads text with spaces to center it within given width |

---

## 3. `examples/games/block_world.eng`

Text grid exploration game.

**Map:** 8×8 flat `list of standard number`. Cell codes:
- 0 = air (`.`)
- 1 = dirt (`D`)
- 2 = tree (`T`)
- 3 = stone (`S`)
- 4 = water (`~`)

Player displayed as `@`. Position tracked as `playerRow` and `playerCol`.

**Mod zone (top of file):**
```
let MAP_SIZE be 8
let START_WOOD be 0
let START_STONE be 0
let TREE_CHANCE be 30
let STONE_CHANCE be 15
```

**Commands:** `north`, `south`, `east`, `west`, `mine`, `inventory`, `quit`

**Game loop:** While loop reads a command each turn, dispatches to handler functions, redraws the map.

**Student gap:** `craft` function is stubbed — students implement a recipe (2 wood → 1 plank).

---

## 4. `examples/games/kart_race.eng`

Turn-based racing game for 4 karts.

**State:** `list of standard number` for positions (indices 0–3 = player + 3 CPUs). Separate list tracks held items (0=none, 1=mushroom, 2=banana, 3=blue shell).

**Mod zone (top of file):**
```
let TRACK_LENGTH be 30
let PLAYER_NAME be "You"
let CPU1_NAME be "Bowser"
let CPU2_NAME be "Peach"
let CPU3_NAME be "Toad"
let ITEM_CHANCE be 40
```

**Player actions per turn:** `gas`, `item`, `quit`

**CPU behavior:** Each CPU advances by a random 1–3 spaces per turn.

**Items:**
- Mushroom (code 1): player advances +3 bonus
- Banana (code 2): next CPU to move loses 2 spaces — implemented as the example
- Blue Shell (code 3): stubbed for students

**Student gap:** `useItem` has Mushroom and Banana implemented; Blue Shell is a clearly marked stub.

**Win condition:** First kart to reach or pass `TRACK_LENGTH` wins. Display final standings.

---

## 5. Docs

- `docs/language-reference/game-package.md` — beginner-friendly doc for the `game` package
- `docs/language-reference/index.md` — updated to include the game package entry

---

## File list

```
src/stdlib.rs                          ← add clearScreen, repeatText
src/codegen.rs                         ← implement clearScreen, repeatText
packages/game/game.eng                 ← new package
packages/game/README.md                ← new
examples/games/block_world.eng         ← new
examples/games/kart_race.eng           ← new
docs/language-reference/game-package.md ← new
docs/language-reference/index.md       ← updated
```
