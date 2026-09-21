# Tensors

A **tensor** is a grid of numbers, like a spreadsheet. Neural networks are built out of tensors: the inputs are a tensor, every layer's weights are a tensor, and the answers come out as a tensor.

## Making a tensor

Write a list, or a list of lists, and store it as a `tensor`:

```
let prices be a tensor with value [1.5, 2.0, 0.75].
let orders be a tensor with value [[2, 1, 0], [0, 3, 2], [1, 1, 1]].
```

`prices` is a row of 3 numbers. `orders` has 3 rows and 3 columns. Every row needs the same number of numbers.

You can also make a tensor by saying how big it is:

| Write | You get |
|---|---|
| `the result of zeroTensor with 2 and 3` | 2 rows and 3 columns, all `0.0` |
| `the result of oneTensor with 4` | 4 numbers, all `1.0` |
| `the result of randomTensor with 2 and 3` | 2 rows and 3 columns of random numbers |

Random tensors follow a bell curve: most numbers are close to 0, and a few are further away. They start the same way every time you run your program, so you and your friends see the same numbers.

## Printing a tensor

```
output orders.
```

```
[ 2.0  1.0  0.0 ]
[ 0.0  3.0  2.0 ]
[ 1.0  1.0  1.0 ]
```

## Maths on tensors

`+`, `-`, `*` and `/` work number by number:

```
let doubled be a tensor with value orders * 2.
let fewer be a tensor with value orders - 1.
```

When the two tensors are different sizes, the smaller one is **stretched** to fit. This is called **broadcasting**:

```
output orders * prices.
```

```
[ 3.0  2.0   0.0 ]
[ 0.0  6.0   1.5 ]
[ 1.5  2.0  0.75 ]
```

`prices` has one number per column, so each row of `orders` is multiplied by the same prices.

Broadcasting lines the sizes up from the right. Each pair has to be the same, or one of them has to be 1. If they don't fit, eng-lish tells you which sizes clash:

```
Error: You tried to add a 3-by-2 tensor and a 4-by-1 tensor. Line their sizes up from the right: 3 and 4 don't match. Each pair needs to be the same, or one of them needs to be 1.
```

## Matrix multiplication

`matmul` multiplies each row of the first tensor by the second one and adds up the answer. It's how a neural network layer mixes its inputs together.

```
output the result of matmul with orders and prices.
```

```
[ 5.0  7.5  4.25 ]
```

That's each friend's whole bill: `2 × 1.5 + 1 × 2.0 + 0 × 0.75 = 5.0`, and so on.

For `matmul`, the *width* of the first tensor has to match the *height* of the second. A 4-by-2 tensor times a 2-by-3 tensor gives a 4-by-3 tensor.

## Reading parts of a tensor

| Write | You get |
|---|---|
| `orders[0]` | The first row, as a tensor |
| `orders[0][1]` | One number, as a single-number tensor |
| `the value of t` | A single-number tensor as a normal decimal |
| `the shape of orders` | Its sizes, as a list of standard numbers |
| `the sum of orders` | Everything added up |
| `the mean of orders` | The average of everything |
| `the transpose of orders` | Rows turned into columns |

`the value of` only works on a tensor with one number in it, so pick the number out first:

```
let cell be a tensor with value orders[0][1].
output the value of cell.
```

To add up in just one direction, use `sumAlong` or `meanAlong` with a direction. Direction `0` goes down the columns, and direction `1` goes across the rows:

```
output the result of sumAlong with orders and 0.
```

```
[ 3.0  5.0  3.0 ]
```

`reshape` keeps the same numbers but changes the grid:

```
output the result of reshape with orders and 9.
```

## Activation functions

`sigmoid`, `relu`, `tanh`, `exponential`, `logarithm` and `softmax` work on every number in a tensor. `power` raises every number to a power:

```
let squashed be a tensor with value the result of sigmoid with orders.
let squared be a tensor with value the result of power with orders and 2.
```

`softmax` turns each row into chances that add up to 1.

## A neural network layer

Put it together and you have a neural network, with no sizes to keep track of by hand:

```
let inputs be a tensor with value [[0.0, 0.0], [0.0, 1.0], [1.0, 0.0], [1.0, 1.0]].
let weights be a tensor with value the result of randomTensor with 2 and 3.
let biases be a tensor with value the result of zeroTensor with 3.
let hidden be a tensor with value the result of tanh with (the result of matmul with inputs and weights) + biases.
output hidden.
```

See `examples/forward_pass.eng` for a two-layer network.

Tensors can go into and out of your own functions, but they can't go inside lists or kinds yet.
