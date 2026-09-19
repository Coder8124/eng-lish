# Watched Decimals and Gradients

This is how machines learn. A **watched decimal** is a decimal that remembers how it was worked out. Because it remembers, eng-lish can answer a very useful question: *if I nudge this number a little, how much does the answer change?* That answer is called the **gradient**.

## Making a watched decimal

Write `watched decimal` where you would write `decimal`:

```
let width be a watched decimal with value 3.0.
let height be a watched decimal with value 2.0.
let area be a watched decimal with value width * height.
```

`area` is `6.0`, just like a normal decimal. The difference is that it remembers it came from `width` times `height`.

Anything worked out from a watched decimal must be a watched decimal too. If you write `let area be a decimal with value width * height.`, eng-lish tells you to make `area` watched.

## Finding the gradients

```
Find the gradients of area.
output the gradient of width.
output the gradient of height.
```

This prints `2.0` and then `3.0`. Here is what they mean:

- The gradient of `width` is `2.0`. Make the width 1 bigger and the area grows by 2, because the height is 2.
- The gradient of `height` is `3.0`. Make the height 1 bigger and the area grows by 3, because the width is 3.

`Find the gradients of` works out the gradient of *every* watched decimal that the answer came from, all at once. Each time you use it, the old gradients are cleared first.

To get a watched decimal back as a normal decimal, ask for its value:

```
let size be a decimal with value the value of area.
```

## Seeing how it was worked out

`show the graph of` prints every step, with each step's gradient:

```
show the graph of area.
```

```
area = 6.0  (times)   gradient 1.0
├── width = 3.0   gradient 2.0
└── height = 2.0   gradient 3.0
```

Read it from the bottom up to see how the answer was built. Read it from the top down to see how the gradients flow back. This flowing back is called the **chain rule**, and it is how every neural network learns.

A really big graph stops after 200 lines, so your screen doesn't fill up.

## What you can do with watched decimals

| You can write | Example |
|---|---|
| `+`, `-`, `*`, `/` with watched decimals or normal numbers | `w * 3.0 - 6.0` |
| `negative` | `negative w` |
| Compare them | `If loss is less than 0.01,` |
| `sigmoid`, `relu`, `tanh`, `exponential`, `logarithm` | `the result of sigmoid with w` |
| `power` with a normal number | `the result of power with w and 2` |
| `Add`, `Subtract`, `Multiply`, `Divide` sentences | `Subtract 0.1 from w.` |
| Pass them to your own functions and give them back | `To squaredMiss with a watched decimal guess ...` |

Watched decimals can't go inside lists or kinds yet.

## Learning: rolling downhill

Imagine the **loss** is a number that says how wrong a guess is. Lower is better. The gradient tells you which way is *uphill* for the loss, so you take a small step the other way. Do that many times and you roll down to the best answer. This is called **gradient descent**.

This program learns the number whose square is 9:

```
let guess be a watched decimal with value 1.0.

For each step from 1 to 50,
    let loss be a watched decimal with value (guess * guess - 9.0) * (guess * guess - 9.0).
    Find the gradients of loss.
    Subtract 0.01 * the gradient of guess from guess.
End.

output guess.
```

It prints `3.0`.

`Subtract 0.01 * the gradient of guess from guess.` gives `guess` its new value, so the next round builds its loss from the new guess. The `0.01` is the **step size** (also called the learning rate). Too big and the guess jumps right past the answer. Too small and learning takes forever. Try changing it!

For a bigger example, see `examples/gradient_descent.eng`. It learns the slope and starting point of a line from four points.
