# Development container

A ready-made environment for working on eng-lish, so nobody has to install
LLVM 21.1 by hand.

## Using it

**In the browser** — open [a Codespace](https://codespaces.new/Coder8124/eng-lish).

**Locally** — with Docker and VS Code installed, open the repo and choose
**Reopen in Container** when prompted.

Either way you get Rust, LLVM 21.1, clang, the built compiler, and the
playground running on port 8080.

## What is here

| File | What it does |
|---|---|
| `Dockerfile` | Debian + Rust image, with LLVM 21 installed from apt.llvm.org and `LLVM_SYS_211_PREFIX` set |
| `devcontainer.json` | Wires up the build, the port forward, and rust-analyzer |
| `setup.sh` | Runs once: builds the compiler and the playground, and links the VS Code extension from `editors/vscode` so `.eng` files highlight |
| `start-playground.sh` | Runs on attach: starts the playground unless it is already up |

The LLVM version is pinned because `inkwell` exposes one feature per LLVM major
version and the crate is built against `llvm21-1`. To move to a new LLVM, change
`LLVM_VERSION` here, the `ENV LLVM_SYS_*_PREFIX` line, and the `inkwell` feature
in the root `Cargo.toml` together.

## If something goes wrong

The playground writes to `.devcontainer/logs/playground.log`. To restart it:

```bash
pkill -f playground/target/release/playground
.devcontainer/start-playground.sh
```
