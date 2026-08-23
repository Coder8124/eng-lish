# Contributing to eng-lish

eng-lish is a compiled programming language whose syntax is English sentences. It is built for people learning to program — roughly ages 10 to 16 — and it grows from there into real software, data science, and even programs that run on a TI-83 graphing calculator.

Contributions are welcome, whether that is a bug report, a documentation page, a new package, or a change to the compiler itself.

## What you need first

| You need | Why |
|---|---|
| [Rust](https://rustup.rs/) 1.85 or newer | The compiler is written in Rust and uses edition 2024 |
| LLVM **21.1** | `inkwell` is pinned to the `llvm21-1` feature, and that pin is exact |
| `clang` | eng-lish uses it to link the finished program |

LLVM pins matter more than usual here. The `inkwell` crate exposes one feature per LLVM major version, so **LLVM 22 will not work and neither will LLVM 20** — it has to be 21.1.

On macOS:

```bash
brew install llvm@21
export LLVM_SYS_211_PREFIX=/opt/homebrew/opt/llvm@21
```

On Debian/Ubuntu:

```bash
sudo apt install llvm-21 llvm-21-dev clang
export LLVM_SYS_211_PREFIX=/usr/lib/llvm-21
```

Put that `export` line in your shell profile so you do not have to repeat it.

## Build it and run something

```bash
git clone https://github.com/Coder8124/eng-lish.git
cd eng-lish
cargo build
```

That gives you `target/debug/englishc`. Compile and run a program with it:

```bash
./target/debug/englishc examples/hello.eng
./examples/binaries/hello
```

There is also a shortcut script that compiles, runs, and cleans up after itself:

```bash
./engine examples/hello.eng
./engine hello.eng          # bare names are looked up in examples/
```

Run the tests:

```bash
cargo test
```

Run the browser playground:

```bash
cargo build                 # build the compiler first, from the repo root
cd playground
cargo run                   # then open http://127.0.0.1:8080
```

## What lives where

| Folder | What is in it |
|---|---|
| `src/` | The compiler, in Rust |
| `packages/` | The standard library, written in eng-lish itself |
| `examples/` | Runnable `.eng` programs, including games |
| `docs/language-reference/` | Docs that teach a feature |
| `docs/spec/` | The language specification — what the parser actually accepts |
| `playground/` | The browser playground (a separate crate) |
| `editors/vscode/` | Syntax highlighting and snippets for VS Code |

Inside `src/`, a program flows through the files in this order:

| File | Job |
|---|---|
| `lexer.rs` | Turns text into tokens. Every keyword lives here |
| `parser.rs` | Turns tokens into a tree, by recursive descent |
| `ast.rs` | The shape of that tree |
| `semantic.rs` | Checks types, resolves beginner-mode inference, reports friendly errors |
| `codegen.rs` | Emits LLVM IR — by far the biggest file |
| `stdlib.rs` | The table of built-in functions |
| `tibasic.rs` | The alternate backend that produces TI-BASIC for calculators |
| `main.rs` | The command line, `use` resolution, and linking |

## Where to make your change

**Adding a package is the easiest place to start, and needs no Rust at all.** Package lookup is purely filesystem-based (`src/main.rs:25-37`), so a new file is all it takes:

1. Write `packages/<name>/<name>.eng`. A package holds only `To …:` functions and `Define a kind called …` classes — top-level statements in a package are ignored when it is imported.
2. Write `packages/<name>/README.md` describing every function, its types, and an example.
3. Add a row to the package table in `docs/language-reference/index.md` and to "Bundled packages" in `README.md`.

Use it from a program with `use "<name>".` — note that this only resolves against `packages/` when you run `englishc` from the repo root.

**Adding a built-in function** means two files: register it in `src/stdlib.rs` (in whichever `get_*_builtins` group fits) and implement it in `src/codegen.rs`.

**Adding new syntax** is the big one, and usually touches five files in a row: `lexer.rs` for the keyword, `parser.rs` for the sentence shape, `ast.rs` for the node, `semantic.rs` for the type rules, and `codegen.rs` to emit it. Add it to `tibasic.rs` too if it is something a calculator could do. It also needs a `docs/spec/` update, because the spec is supposed to describe the real parser.

## Rules that are easy to trip over

- `While` and `For each` **must** close with `End.`. After `If`, `End.` is optional — that asymmetry is real, not a typo.
- Prefer the natural compound forms: `Add X to Y.`, `Subtract X from Y.`, `Multiply Y by X.`, `Divide Y by X.`. The older `Multiply X to Y.` still parses but should not appear in new code.
- Prefer `the remainder of X divided by Y` over the short `remainder X by Y`.
- Single-letter variable names collide with the article keyword `a`. Use descriptive names.
- `Type::Inferred` must never reach code generation — call `analyzer.patch_program_types(&mut program)` before you generate code.
- Only write example code using syntax that actually exists. If you are unsure, compile it before you commit it. `docs/spec/` is the reference for what is real.

## Documentation is part of the change

A feature is not finished until someone can learn it from the docs:

- Every new language feature gets a page in `docs/language-reference/`, and a row in `docs/language-reference/index.md`.
- Every new package gets a `README.md` in its own folder.
- Write for a 12-year-old meeting the idea for the first time: plain English, short sentences, and examples that run.
- Do not document features that do not exist yet.

## Tests

Tests live in a `#[cfg(test)] mod tests` block at the bottom of the file they test — see the blocks at the end of `lexer.rs`, `parser.rs`, `semantic.rs`, and `tibasic.rs`. Add yours to the file you changed and make sure `cargo test` passes before you open a pull request.

## Commits and pull requests

- One commit per logical change, not one big commit at the end.
- Subject line in the imperative mood, capitalized, no type prefix, no trailing period: `Add dictionary literals`, `Fix off-by-one in list indexing`.
- If a change needs explaining, leave a blank line after the subject and use dash bullets.
- Say in the pull request what you changed, how you tested it, and which docs you updated.

## Good first contributions

Real gaps, if you are looking for somewhere useful to start:

- **Write a missing reference page.** `docs/language-reference/index.md` lists Variables, Functions, Loops, Classes, Strings, and File I/O, but those pages do not exist yet.
- **Write a missing package README.** `algorithm`, `math`, `numeric`, and `strings` have no `README.md`.
- **Make compiler errors clickable.** `englishc` prints `Line 12: …` with no file name, so editors cannot jump to the error. Printing `file:line: …` would make the VS Code problem matcher work.
- **Test the code generator.** `src/codegen.rs` is the largest file in the project and has no tests at all.
- **Add an end-to-end test** that compiles every program in `examples/` and checks it still runs.

## License

By contributing, you agree that your contributions are licensed under the [MIT License](LICENSE), the same as the rest of the project.
