# neural package

The `neural` package helps you build and run neural networks in eng-lish. A neural network is a program that learns from examples — kind of like how your brain learns to recognize faces by seeing lots of them.

## How to use it

Add this line at the top of your program:

```
use "neural".
```

---

## The easy way: `NeuralNetwork`

Most of the time you don't need any of the functions below — just use the `NeuralNetwork` class. It builds an input layer, one hidden layer, and an output layer, and knows how to train itself with backpropagation.

```
let net be a NeuralNetwork created with 2 and 4 and 1 and "relu" and "sigmoid" and 0.5.
```

Arguments: `inSize`, `hiddenSize`, `outSize`, `hiddenActivation` (`"relu"` or `"sigmoid"`), `outputActivation`, `learningRate`.

### `predict`

Runs one forward pass and returns the network's output.

```
the result of asking net to predict with input
```

### `trainOnExample`

Trains the network on one input/target pair — one forward pass, one backward pass, one weight update. Returns the loss.

```
the result of asking net to trainOnExample with input and target
```

### `trainNetwork`

A free function that loops `trainOnExample` over every example, for as many epochs as you ask for.

```
Call trainNetwork with net and inputs and targets and numExamples and epochs.
```

- `inputs` / `targets` — a `list of list of decimal`, one row per training example
- `numExamples` — how many rows they have
- `epochs` — how many times to practice on the whole set

See `examples/neural_network_training.eng` for a full working example that trains a network to learn the AND gate.

---

## Backpropagation building blocks

`NeuralNetwork` uses these under the hood. Reach for them directly only if you're writing your own training loop.

| Function | What it does |
|----------|-------------|
| `elementwiseMultiply with vecA and vecB and n` | Multiplies two lists position by position |
| `outerProduct with vecA and vecB and sizeA and sizeB` | Turns two lists into a weight-shaped gradient |
| `matTransposeVecMul with matrix and vec and rows and cols` | Sends error backward through a layer's weights |
| `sigmoidDerivativeFromOutput with vec and n` | The slope of sigmoid, from its output |
| `reluDerivativeFromZ with vec and n` | The slope of relu, from its input |
| `applyActivation with vec and kind` | Runs the activation named by `kind` (`"relu"`, `"sigmoid"`, `"softmax"`) |
| `activationDerivative with preActivation and postActivation and kind and n` | Picks the right derivative function by name |

---

## Low-level functions

These are the individual pieces a layer is made of — what `NeuralNetwork` calls internally.

### `initWeights`

Creates a list of random starting numbers (weights) for a layer.

```
initWeights with inSize and outSize
```

- `inSize` — how many inputs the layer receives
- `outSize` — how many outputs the layer produces

### `initBiases`

Creates a list of zeros (biases) for a layer.

```
initBiases with size
```

- `size` — how many biases to create (should equal `outSize` of the layer)

### `linearLayer`

Runs one layer of the network: multiplies inputs by weights and adds biases.

```
linearLayer with weights and biases and inputs and outSize and inSize
```

### `sigmoidActivation`

Squishes every number in a list to be between 0 and 1. Good for output layers.

```
sigmoidActivation with vec
```

### `reluActivation`

Sets negative numbers to 0, keeps positive numbers the same. Good for hidden layers.

```
reluActivation with vec
```

### `softmaxActivation`

Turns a list of numbers into probabilities that all add up to 1. Good for classification.

```
softmaxActivation with vec
```

### `mseError`

Measures how wrong your network's predictions are (lower is better).

```
mseError with predictions and targets
```

### `updateWeights`

Moves weights a small step in the right direction to reduce error.

```
updateWeights with weights and gradients and learningRate
```

- `learningRate` — how big each step is (try `0.01` to start)

---

## Low-level example: manual forward pass, no training

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

This creates a small network with:
- 2 inputs
- 1 hidden layer with 4 neurons (sigmoid activation)
- 1 output (sigmoid activation)
- Mean squared error loss
