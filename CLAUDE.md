# Claude Conventions for eng-lish

## What we're building

eng-lish is a compiled, English-syntax programming language targeting beginner programmers (ages 10–16) and problem-solvers, while scaling into more advanced domains. The full vision:

- **Beginner mode** — simplified syntax (`use beginner.`), untyped parameters inferred at call sites, `Oops!`-style kid-friendly error messages
- **General software development** — typed functions, classes, control flow, file I/O, package system, competitive-programming-style problem solving
- **ML / AI / data science** — native packages for vectors, matrices, stats, plotting (already started with the `plot` statement and numeric package); goal is to let beginners do real data science without leaving the language
- **TI-BASIC compiler target** — a library/package addon that compiles eng-lish programs to TI-BASIC for graphing calculators, so students can run their code on their calculators for math class
- **Release-ready** — the language must be properly documented so it can be shipped as a standalone product at any time

## Stack

- Rust (compiler implementation)
- `logos` (lexer)
- `inkwell` / LLVM (native code generation)
- `clang` (linking)
- `cargo test` (unit tests in `#[cfg(test)]` modules inside each source file)

## Folder structure

```
src/                  # Rust compiler source
  main.rs             # CLI entry point, error routing
  lexer.rs            # logos-based lexer
  parser.rs           # Recursive-descent parser
  ast.rs              # AST node types
  semantic.rs         # Type checker / semantic analyzer
  codegen.rs          # LLVM IR code generation via inkwell
  stdlib.rs           # Built-in function registry
packages/             # eng-lish standard library packages
  numeric/            # Math utilities
  algorithm/          # Sorting, searching, etc.
  geometry/           # Geometric calculations
examples/             # Example .eng programs (one per feature area)
docs/                 # Language documentation (see Documentation section)
  language-reference/ # Spec-level docs for each language feature
  tutorials/          # Step-by-step guides for beginners
  release-notes/      # Changelog per version
```

## Documentation requirements

**Claude must create and maintain documentation** so the language is ready to ship at any time:

- Every new language feature needs a corresponding doc in `docs/language-reference/` as a Markdown file (e.g., `beginner-mode.md`, `functions.md`, `loops.md`).
- Every new package needs a `README.md` inside its package directory describing its functions, types, and example usage.
- When a feature is complete, update `docs/language-reference/index.md` (create it if it doesn't exist) with a link and one-line summary.
- Write docs from the perspective of a 12-year-old reading them for the first time — plain English, short sentences, runnable examples.
- Do **not** write docs speculatively for features that don't exist yet.

## Key constraints

- Only use language constructs that are verified to exist in the parser — don't write `.eng` examples using syntax that hasn't been implemented.
- `While` loops require `End.` to close (multi-statement body, same as `For each`).
- Compound assignment syntax: `Add X to Y.` / `Subtract X from Y.` / `Multiply X to Y.` / `Divide X from Y.` — NOT `Multiply Y by X`.
- Single-letter variable names (e.g., `a`, `b`) conflict with the article token — use descriptive names.
- `Type::Inferred` must never reach codegen — always call `analyzer.patch_program_types(&mut program)` before code generation.
- Never add `Co-Authored-By` or any Claude attribution to commit messages.

## Commits

- Commit after every response that makes changes — one commit per logical change, not one giant commit at the end.
- Keep commit messages concise: a short subject line, then a blank line, then bullet points if needed.

## Code style

- No comments unless the why is genuinely non-obvious.
- No docstrings or multi-line comment blocks.
- Don't add error handling for things that can't happen.
- Don't introduce abstractions beyond what the task requires.

## eng-lish packages

- New packages go in `packages/<name>/<name>.eng`.
- Only use language constructs visible in the existing examples — don't guess at unverified syntax.
- Packages export functions and classes only; top-level statements in a package file are ignored at import time.

## General

- Don't rewrite existing files unless explicitly asked.
