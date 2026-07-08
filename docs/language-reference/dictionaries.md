# Dictionaries

A dictionary stores values under names, the same way a real dictionary stores definitions under words. The names are called **keys**. In eng-lish, keys are always text.

## Making a dictionary

```
Let ages be a new dictionary.
```

This makes an empty dictionary where every value is a standard number. Want to store decimals or text instead? Say so:

```
Let prices be a new dictionary from text to decimal.
Let colors be a new dictionary from text to text.
```

You can also write `lock and key list` instead of `dictionary` — they mean the same thing.

## Putting things in

Use square brackets with the key, just like lists:

```
Set ages["Alice"] to 12.
Set ages["Bob"] to 14.
```

If the key is already there, its value gets replaced:

```
Set ages["Alice"] to 13.
```

## Getting things out

```
output ages["Alice"].
```

If the key isn't in the dictionary, you get 0 (or empty text). Check first with `hasKey` if you're not sure:

```
If the result of hasKey with ages and "Alice" then
    output ages["Alice"].
End.
```

## Taking things out

```
Remove "Bob" from ages.
```

## Helpful functions

| Function | What it does | Example |
|----------|--------------|---------|
| `hasKey` | Is this key in the dictionary? | `the result of hasKey with ages and "Alice"` |
| `sizeOf` | How many entries are there? | `the result of sizeOf with ages` |
| `keysOf` | A list of text with every key | `the result of keysOf with ages` |

## A full example

```
Let prices be a new dictionary from text to decimal.
Set prices["apple"] to 1.5.
Set prices["banana"] to 0.75.

output prices["apple"] + prices["banana"].

Let fruit be a list of text with value the result of keysOf with prices.
For each i from 0 to 1,
    output fruit[i].
End.
```

This prints the total price, then each fruit name.

Try the full runnable example in `examples/dictionaries.eng`.
