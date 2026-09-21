# Neural Networks

The `neural` package lets you build neural networks in eng-lish. You don't need to know calculus — the package handles the math for you.

## What is a neural network?

A neural network is a program made of layers. Each layer takes some numbers in, does some math, and passes numbers to the next layer. After seeing lots of examples, the network learns to make good predictions.

## Getting started

Put this at the top of your program:

```
use "neural".
```

## The easy way: `NeuralNetwork`

`NeuralNetwork` is a class that builds a whole network — an input layer, one hidden layer, and an output layer — and knows how to train itself. This is the fastest way to go from nothing to a working, learning network.

```
let net be a NeuralNetwork created with 2 and 4 and 1 and "relu" and "sigmoid" and 0.5.
```

That one line creates a network with:

- `2` — how many numbers go in (inputs)
- `4` — how many neurons in the hidden layer
- `1` — how many numbers come out (outputs)
- `"relu"` — the hidden layer's activation function
- `"sigmoid"` — the output layer's activation function
- `0.5` — the learning rate (how big each learning step is)

### Training it

Gather your training examples as two lists — one of inputs, one of matching answers — then call `trainNetwork`:

```
let x0 be a list of decimal with value [0.0, 0.0].
let x1 be a list of decimal with value [0.0, 1.0].
let x2 be a list of decimal with value [1.0, 0.0].
let x3 be a list of decimal with value [1.0, 1.0].
let inputs be a list of list of decimal with value [x0, x1, x2, x3].

let y0 be a list of decimal with value [0.0].
let y1 be a list of decimal with value [0.0].
let y2 be a list of decimal with value [0.0].
let y3 be a list of decimal with value [1.0].
let targets be a list of list of decimal with value [y0, y1, y2, y3].

Call trainNetwork with net and inputs and targets and 4 and 2000.
```

The last two arguments are how many examples you gave it (`4`) and how many times to practice on all of them (`2000` epochs). Behind the scenes, `trainNetwork` runs the network forward, checks how wrong it was, and nudges every weight a little closer to correct — that's called **backpropagation**, and this function does it for you, one example at a time, every epoch.

### Making predictions

Once it's trained (or even before, to see how bad an untrained guess is):

```
let prediction be a list of decimal with value the result of asking net to predict with x3.
output prediction[0].
```

### If you want to train one example yourself

`trainNetwork` is just a loop around `trainOnExample`, which you can call directly if you want more control (say, to print the loss as it trains):

```
let loss be a decimal with value the result of asking net to trainOnExample with x0 and y0.
output loss.
```

`trainOnExample` runs one forward pass, computes the loss, computes every gradient, and updates every weight and bias — all in one call. It returns the loss so you can watch it go down as you call it more.

## How it works under the hood

`NeuralNetwork` is built out of smaller pieces that are also available on their own, if you want to build a network by hand instead of using the class.

### Layers

A layer needs **weights** (the things it learns) and **biases** (extra adjustments).

```
let w1 be a list of decimal with value the result of initWeights with 2 and 4.
let b1 be a list of decimal with value the result of initBiases with 4.
```

This creates a layer: 2 inputs going into 4 hidden neurons.

### Running a forward pass

A forward pass means feeding data through the network to get a prediction.

```
let hidden be a list of decimal with value the result of linearLayer with w1 and b1 and input and 4 and 2.
let hiddenOut be a list of decimal with value the result of sigmoidActivation with hidden.
```

1. `linearLayer` multiplies your input by the weights and adds the biases.
2. `sigmoidActivation` squishes the result to be between 0 and 1.

### Measuring error

After the network makes a prediction, check how wrong it was:

```
let loss be a decimal with value the result of mseError with pred and target.
```

A lower loss means the network is doing better.

### Updating weights by hand

`updateWeights` nudges weights toward lower loss, once you already have gradients (from `outerProduct` and the activation-derivative helpers below):

```
let w1 be a list of decimal with value the result of updateWeights with w1 and gradients and 0.01.
```

The third argument (`0.01`) is the learning rate — how big each step is. This is exactly what `trainOnExample` does for you automatically.

### Activation functions

| Function | What it does | When to use it |
|----------|-------------|----------------|
| `sigmoidActivation` | Squishes values to 0–1 | Output layers, binary decisions |
| `reluActivation` | Zeros out negatives | Hidden layers |
| `softmaxActivation` | Makes values sum to 1 (probabilities) | Classification output |

### Backpropagation building blocks

These are the low-level pieces `trainOnExample` uses to compute gradients. You'll only need these if you're building your own training loop instead of using `NeuralNetwork`.

| Function | What it does |
|----------|-------------|
| `elementwiseMultiply` | Multiplies two lists position by position |
| `outerProduct` | Turns two lists into a weight-shaped gradient |
| `matTransposeVecMul` | Sends error backward through a layer's weights |
| `sigmoidDerivativeFromOutput` | The slope of sigmoid, from its output |
| `reluDerivativeFromZ` | The slope of relu, from its input |
| `activationDerivative` | Picks the right derivative function by name (`"relu"` or `"sigmoid"`) |

## Full example: training a network

See `examples/neural_network_training.eng` for a complete program that trains a `NeuralNetwork` to learn the AND gate (`0,0 → 0`, `0,1 → 0`, `1,0 → 0`, `1,1 → 1`) and prints its predictions before and after training.

See also: the full `neural` package documentation in `packages/neural/README.md`.
