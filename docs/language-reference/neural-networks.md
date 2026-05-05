# Neural Networks

The `neural` package lets you build neural networks in eng-lish. You don't need to know math — the package handles everything for you.

## What is a neural network?

A neural network is a program made of layers. Each layer takes some numbers in, does some math, and passes numbers to the next layer. After seeing lots of examples, the network learns to make good predictions.

## Getting started

Put this at the top of your program:

```
use "neural".
```

## Creating a network

A network has layers. Each layer needs **weights** (the things it learns) and **biases** (extra adjustments).

```
let w1 be a list of decimal with value the result of initWeights with 2 and 4.
let b1 be a list of decimal with value the result of initBiases with 4.
```

This creates the first layer: 2 inputs going into 4 hidden neurons.

## Running a forward pass

A forward pass means feeding data through the network to get a prediction.

```
let hidden be a list of decimal with value the result of linearLayer with w1 and b1 and input and 4 and 2.
let hiddenOut be a list of decimal with value the result of sigmoidActivation with hidden.
```

1. `linearLayer` multiplies your input by the weights and adds the biases.
2. `sigmoidActivation` squishes the result to be between 0 and 1.

## Measuring error

After the network makes a prediction, check how wrong it was:

```
let loss be a decimal with value the result of mseError with pred and target.
```

A lower loss means the network is doing better.

## Updating weights

To make the network learn, nudge the weights toward lower loss:

```
let w1 be a list of decimal with value the result of updateWeights with w1 and gradients and 0.01.
```

The third argument (`0.01`) is the learning rate — how big each step is.

## Activation functions

| Function | What it does | When to use it |
|----------|-------------|----------------|
| `sigmoidActivation` | Squishes values to 0–1 | Output layers, binary decisions |
| `reluActivation` | Zeros out negatives | Hidden layers |
| `softmaxActivation` | Makes values sum to 1 (probabilities) | Classification output |

## Full example

```
use "neural".

output "Creating a 2-input, 4-hidden, 1-output network.".

let w1 be a list of decimal with value the result of initWeights with 2 and 4.
let b1 be a list of decimal with value the result of initBiases with 4.
let w2 be a list of decimal with value the result of initWeights with 4 and 1.
let b2 be a list of decimal with value the result of initBiases with 1.

let input be a list of decimal with value [0.5, 0.8].
let target be a list of decimal with value [1.0].

let hidden be a list of decimal with value the result of linearLayer with w1 and b1 and input and 4 and 2.
let hiddenOut be a list of decimal with value the result of sigmoidActivation with hidden.
let finalOut be a list of decimal with value the result of linearLayer with w2 and b2 and hiddenOut and 1 and 4.
let pred be a list of decimal with value the result of sigmoidActivation with finalOut.

output "Prediction (before training):".
let predVal be a decimal with value pred[0].
output predVal.

let loss be a decimal with value the result of mseError with pred and target.
output "Loss:".
output loss.
```

See also: the full `neural` package documentation in `packages/neural/README.md`.
