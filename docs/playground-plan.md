# eng-lish Online Playground — Plan

A browser playground is the front door for eng-lish: a visitor should go from "what is this?" to running their first program in under ten seconds, with zero install. This document is the implementation plan.

> **Status:** v1 is implemented in `playground/` (milestones 1–5: run API, editor UI, example gallery, share links, TI-BASIC tab, plot tab, syntax highlighting). Milestone 6 (public deploy behind a real sandbox) is still open — see `playground/README.md`.

## Goals

1. **Show what the language actually does.** The first screen is a working program, already loaded, with a Run button — not an empty editor.
2. **Zero install, zero sign-up.** Works on school Chromebooks and locked-down lab machines.
3. **Showcase every feature area** through a curated example gallery: beginner mode, functions, classes, lists, dictionaries, plotting, and the TI-BASIC target.
4. **Shareable links** so teachers can hand out a URL that opens a specific program.

## Non-goals (v1)

- Accounts, saved projects, or classrooms — share links cover 90% of the need.
- File I/O and package installation in the browser.
- Mobile editing (mobile *viewing* should work).

## Architecture

Two realistic options:

### Option A — server-side compile & run (recommended for v1)

The compiler stays exactly as it is. A small API service accepts source, runs `englishc` in a sandbox, and returns output.

```
Browser (editor UI)
   │  POST /run { source }
   ▼
API server (Rust, axum)
   │  writes source to tmpfile
   ▼
Sandboxed runner: englishc + execute
   (Docker/gVisor, no network, 256 MB, 5 s CPU, 64 KB output cap)
   │
   ▼
{ stdout, stderr, compile_errors, duration }
```

- **Why**: ships in days, uses the real compiler byte-for-byte, so playground behavior always matches the shipped product. Errors (including `Oops!` beginner errors) are identical.
- **Interactive input**: v1 sidesteps `readLine`/`readNumber` by providing a "program input" textbox whose contents are piped to stdin (this matches how the quadratic example runs in CI).
- **Plotting**: `plot` writes an HTML file; the runner returns it and the UI shows it in an output tab. This is a demo moment nothing else has — keep it.
- **TI-BASIC tab**: a second button, "Show calculator code", calls `/run?target=ti-basic` and displays the generated TI-BASIC side-by-side. Huge differentiator; cheap to build since the flag already exists.

### Option B — compile in the browser (later)

LLVM in WebAssembly is heavy (tens of MB) and inkwell's JIT doesn't target wasm cleanly. A realistic browser-only path is a **tree-walking interpreter** sharing the existing lexer/parser/semantic crates, compiled to wasm. That's a meaningful new component — worth doing eventually for offline use and instant feedback, but not first.

**Decision: build Option A now; structure the crate so lexer/parser/semantic can compile to wasm later** (they already can — only codegen depends on LLVM; keep it that way).

## UI

Single page, three regions:

```
┌────────────────────────────────────────────────────┐
│  eng-lish ▸ [Examples ▾]        [Run ▶] [Calc 📟]  │
├──────────────────────────┬─────────────────────────┤
│                          │  Output                 │
│   Editor                 │  ┌───────────────────┐  │
│   (CodeMirror 6,         │  │ 13                │  │
│    eng-lish syntax       │  │ blue              │  │
│    highlighting)         │  └───────────────────┘  │
│                          │  [Program input…]       │
├──────────────────────────┴─────────────────────────┤
│  ● Try: Hello  Dice game  Quadratic  Dictionaries  │
└────────────────────────────────────────────────────┘
```

- **Example gallery**: sourced directly from `examples/*.eng` at build time so it can never drift from the repo. Each example gets a one-line description and ordering weight in a small manifest file.
- **Syntax highlighting**: a CodeMirror mode generated from the token list in `src/lexer.rs` (keywords are already centralized there).
- **Error display**: compile errors render inline under the offending line; beginner-mode `Oops!` messages get a friendly callout style.
- **Share**: "Share" button encodes the source (lz-string) into the URL fragment — no server storage needed for v1.

## Security

Playground runners execute untrusted native code, so the sandbox is the core of the design, not an afterthought:

- Compile and run inside a per-request container (gVisor or Firecracker preferred over plain Docker).
- No network inside the sandbox; read-only rootfs; tmpfs workdir.
- Hard limits: 5 s wall clock, 256 MB memory, 64 KB captured output, request rate limit per IP.
- The API server never shells out with user-controlled arguments — source goes in a file, filename is server-generated.

## Milestones

| # | Deliverable | Estimate |
|---|-------------|----------|
| 1 | `POST /run` API + sandboxed runner, quadratic example working end-to-end | 2–3 days |
| 2 | Editor page with Run, output pane, stdin box, error display | 2–3 days |
| 3 | Example gallery from `examples/` manifest + share links | 1–2 days |
| 4 | TI-BASIC tab + plot output tab | 1 day |
| 5 | Syntax highlighting + polish pass (mobile viewing, loading states) | 2 days |
| 6 | Deploy (single VM + Caddy, or Fly.io), rate limiting, monitoring | 1–2 days |

Roughly two weeks of focused work to a public v1.

## Open questions

- Hosting budget and domain (playground.eng-lish.org?).
- Do we cap concurrent runs per IP at 1 to keep the VM small?
- Whether the beginner-mode toggle should be a visible switch in the UI or inferred from `use beginner.` (lean: infer, but show a badge when active).
