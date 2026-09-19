# ML Teaching Stack Design

**Date:** 2026-09-18
**Branch:** ml-education
**Status:** Phases 0–2 implemented (`runtime/`, `build.rs`, `src/link.rs`, `runtime/src/watched.rs`, `runtime/src/tensor.rs`); Phases 3–6 not started.

## Goal

Make eng-lish the best place to learn how machine learning actually works, from the theory to a running model — deep enough to teach the real ideas, plain enough for a middle schooler. Students should be able to read an equation in a textbook and find the same steps, in the same order, in their eng-lish code. When they are ready, they should move to PyTorch already knowing its concepts.

eng-lish competes with PyTorch **as a teaching tool**, not as a production framework.

| Compete on | Don't compete on |
|---|---|
| Code that reads like the math | GPU and distributed training |
| Autograd you can see inside | Pretrained model zoos |
| Shape and gradient errors in plain English | Raw throughput at scale |
| Training loops the student writes themselves | Deployment |
| A bridge into PyTorch | |

The closest reference points are micrograd (a tiny, readable autograd engine) and fast.ai's top-down course. The concepts below deliberately mirror PyTorch's, so that the PyTorch export in Phase 6 maps one to one.

---

## Where things stand

- `src/stdlib.rs` / `src/codegen.rs` provide stats, `fitLine`, `kMeans`, vector ops, activations and `plot`.
- `packages/neural` has a `NeuralNetwork` class (one hidden layer, backprop) written in eng-lish.

Four problems block the plan:

1. **Built-ins are hand-written LLVM IR.** Each built-in is built instruction by instruction inside `codegen.rs` (about 9,400 lines). A tensor library with broadcasting and backward passes cannot be written this way.
2. **Nothing is ever freed.** Lists are `malloc`ed and never released. A training loop creating tensors on every step would run out of memory.
3. **Matrices are flat lists with sizes passed by hand**, e.g. `linearLayer with weights and biases and inputs and outSize and inSize`. Wrong sizes silently produce wrong answers.
4. **Some built-ins are wrong.** `kMeans` never updates its centroids and only ever assigns clusters `0` or `k - 1`.

---

## Architecture: the runtime crate

A Rust crate in `runtime/` holds every new built-in. Codegen stops emitting function bodies and emits a declaration plus a call.

```
program.eng ──englishc──▶ program.o ─┐
                                     ├─ clang ─▶ program
runtime/ ──rustc (build.rs)──▶ libenglang_runtime.a ─┘
```

### Build and link

- `build.rs` compiles `runtime/src/lib.rs` with `rustc --crate-type staticlib -C opt-level=3 -C panic=abort` into `OUT_DIR`. It calls `rustc` directly rather than nested `cargo`, to avoid fighting over the target-directory lock. This requires the runtime to have **no crate dependencies**.
- `build.rs` also captures `--print native-static-libs` (e.g. `-lSystem -lc -lm` on macOS, `-lgcc_s -lpthread -ldl …` on Linux) and passes it to `englishc` as a compile-time env var, so linking works on every platform without hard-coding flags.
- `englishc` embeds the archive with `include_bytes!`. It writes the archive next to the object file at link time and deletes it afterwards. `cargo install --path .` therefore still produces a single, self-contained compiler.
- One `link` function in `src/link.rs` is shared by `main.rs` and the codegen tests, so the two can't drift apart.
- The runtime uses `std`. A `no_std` build is smaller, but it needs `rust_eh_personality` stubs and loses easy error printing; the archive size (about 16 MB, embedded in the compiler) doesn't matter here, and the linker drops unused code from programs.

### ABI

- Every exported symbol is `extern "C"`, `#[unsafe(no_mangle)]`, and prefixed `englang_`.
- **Lists** keep today's layout: code holds a pointer to the data, and a `{length: i64, capacity: i64}` header sits 16 bytes before it. Elements are 8 bytes (`i64` or `f64`). The runtime allocates lists with `malloc`, so every existing list operation keeps working on them. `runtime/src/list.rs` is the only Rust code that knows this layout.
- `standard number` is `i64`, `decimal` is `f64`, `boolean` is `i1`/`bool`, and `text` is a NUL-terminated `char*`.
- Tensors and autograd values (Phases 1–3) are **opaque pointers**. Codegen never looks inside them.

### Errors

A runtime error prints a plain-English message to stderr and exits with code 1. Beginner mode will use the same `Oops!` wording that compile errors use. The runtime never panics across the FFI boundary: `panic=abort` guarantees a panic stops the program rather than unwinding into LLVM-generated frames.

### Memory

Lists stay as they are, allocated and never freed, for now. Tensors and autograd values are reference-counted inside the runtime from day one. For watched decimals (Phase 1) the rules are: every value an expression creates is a temporary, released when its statement ends (or before a condition branches); storing into a variable retains the new value and releases the old one; variables live in entry-block slots that start empty, so a `let` inside a loop releases the previous pass's value; parameters are retained on entry, and every slot is released before a function returns, after the returned value has been retained for the caller. Autograd graphs keep their inputs alive through those references. 200,000 training steps run in 1.6 MB with `leaks` reporting nothing. Retrofitting reference counting onto lists is a separate, later project.

---

## Phases

Each phase ships with docs in `docs/language-reference/`, runnable examples, and playground entries, per `CLAUDE.md`. Syntax shown for Phases 1–6 is **proposed** and will change as each phase is designed in detail.

### Phase 0: Runtime crate

- `runtime/` crate, `build.rs`, embedded archive, shared `link` function.
- Port `kMeans` from hand-written IR to Rust as the first built-in, fixing it into a real Lloyd's algorithm (assignment step plus centroid update, repeated until nothing changes or 100 rounds).
- **Done when:** `examples/ml.eng` runs with correct clusters, all tests pass, and the playground still compiles programs.

### Phase 1: Autograd on single numbers (implemented)

The micrograd lesson. The student builds an expression, asks for gradients, and inspects them.

```
let w be a watched decimal with value 0.5.
let loss be a watched decimal with value (w * 3.0 - 6.0) * (w * 3.0 - 6.0).
Find the gradients of loss.
output the gradient of w.
show the graph of loss.
```

- `watched decimal` is a type (`Type::Watched`), an opaque pointer to a runtime `Value` node (operation, children, value, gradient, optional name). Arithmetic with any watched operand builds a node; `sigmoid`, `relu`, `tanh`, `exponential`, `logarithm` and `power` have watched versions.
- `Find the gradients of` resets every gradient in the graph and walks it in reverse topological order. Gradients do **not** accumulate across calls, unlike PyTorch; there is no `zero_grad` for a beginner to forget. The PyTorch export (Phase 6) will emit `zero_grad()` to match.
- `Subtract 0.1 * the gradient of w from w.` changes a declared watched decimal in place, so it stays the same leaf across training steps. On a derived value the same sentence builds a new node.
- `show the graph of` prints the graph as an indented text tree rather than an HTML diagram: it works in the terminal and the playground with no extra tab, and a shared step is printed once and then marked "(shown above)".
- Storing a watched result in a plain `decimal` is a compile error that tells the student to make it watched; asking for the gradient of a plain decimal explains that only watched decimals have one. Watched decimals can't go in lists, dictionaries or kinds yet; that needs element retain/release and waits for tensors.
- **Done:** `examples/gradient_descent.eng` fits `y = 2x + 1` by hand-written gradient descent on watched decimals, and every runtime backward rule is checked against finite differences.

### Phase 2: Tensors (implemented)

A new `tensor` type backed by a reference-counted runtime `Tensor` (shape and contiguous data).

```
let inputs be a tensor with value [[0.0, 1.0], [1.0, 0.0]].
let weights be a tensor with value the result of randomTensor with 2 and 3.
let hidden be a tensor with value the result of relu with (the result of matmul with inputs and weights) + [0.5, 0.0, 0.1].
```

- Lists (nested to any depth, of decimals or whole numbers) and plain numbers convert to a tensor wherever one is expected, so there is no separate "from list" call.
- Constructors are `zeroTensor`, `oneTensor` and `randomTensor`, because `zeros` and `ones` already name list built-ins. `randomTensor` is standard normal from a fixed seed, so every student sees the same numbers.
- Element-wise `+ - * /` with NumPy-style broadcasting; `matmul` for 1-D and 2-D; `transpose`, `reshape`, `sumAlong`, `meanAlong`, `the sum of`, `the mean of`; `sigmoid`, `relu`, `tanh`, `exponential`, `logarithm`, `softmax` and `power` overload on a tensor first argument; `t[i]` drops the first dimension.
- Data is always contiguous: transpose and indexing copy rather than keep strides. Teaching-size tensors don't need views, and copying keeps Phase 3's backward rules simple.
- Shape errors come from the runtime in plain English and teach the rule, e.g. *"You tried to matmul a 3-by-2 tensor by a 4-by-1 tensor. The 2 and the 4 need to match: each row of the first is multiplied by each column of the second, so they need the same length."*
- Codegen's retain/release machinery from Phase 1 now covers any runtime-managed type (watched decimals and tensors), and top-level variables are released when `main` returns.
- **Done:** `examples/forward_pass.eng` runs a two-layer forward pass without passing any sizes by hand.

### Phase 3: Tensor autograd

`Find the gradients of` works on tensors. Every tensor op has a backward rule in the runtime, tested against finite differences.

### Phase 4: Layers, optimizers, training loop

- Layers are kinds: `Linear`, `ReLU`, `Sigmoid`, `Sequential`.
- Optimizers are kinds written in eng-lish: `SGD` first, then `Adam`, short enough to read side by side with the paper.
- Losses: MSE and cross-entropy.
- The student writes the training loop (forward pass, loss, backward pass, optimizer step). There is no one-line `fit` as the main way.
- **Done when:** the existing `NeuralNetwork` class can be rewritten on top of these pieces in fewer lines.

### Phase 5: Data

`readCSV`, a text `split` built-in, `Dataset`, shuffling, mini-batches, and a train/test split. Small datasets ship as a package: XOR, Iris, and a subset of MNIST.

### Phase 6: PyTorch bridge

`englishc --pytorch` translates a program into equivalent Python/PyTorch, the same way `--ti-basic` targets calculators (`src/tibasic.rs` is the model). The playground shows both side by side. This is how students graduate: they already know what each PyTorch line means.

### Throughout: lesson track

Derivative → chain rule → gradient descent → linear regression → logistic regression → a multi-layer network → a small CNN. Each lesson is one playground example and one doc page.

---

## Open questions

- How codegen emits release calls for tensors that escape through `Give back` or are stored in class properties.
- Whether `tensor` carries its shape in the type (`tensor of shape 3 by 2`) for compile-time checks, or checks shapes only at runtime. Runtime-only checking is simpler and is the Phase 2 default.
- Whether the TI-BASIC target should reject tensors outright or support small ones through calculator matrices (`[A]`–`[J]`).
