# eng-lish Playground

A browser playground for eng-lish: write a program, press Run, see the output. No install needed. This is the v1 from `docs/playground-plan.md` (Option A — the server runs the real compiler).

## Running it locally

```
cargo build                 # build the compiler first (repo root)
cd playground
cargo run
```

Then open http://127.0.0.1:8080.

## What it does

- **Run ▶** compiles your program with `englishc` and runs it, showing the output. Ctrl+Enter (Cmd+Enter on Mac) works too.
- **Program input** box: whatever you type there is fed to `readLine` / `readNumber`, one answer per line.
- **Play 🎮** runs your program interactively: output streams in live, and you type answers in the box below the output while it runs. This is how you play the games in `examples/games/` (Pong, Kart race, Block world) — press Play, then type your move each turn. Press Stop ⏹ to end early.
- **Calculator 📟** translates your program to TI-BASIC (same as `englishc --ti-basic`) and shows it in the Calculator tab.
- **Plots tab** appears when your program uses `plot` — the charts render right in the page.
- **Share** copies a link with your program encoded in the URL. No account, no server storage.
- **Examples** come from `playground/examples.json`, which points at files in `examples/` — so they can never drift from the repo.

## API

- `GET /` — the editor page (a single self-contained HTML file, `static/index.html`).
- `GET /examples` — the example gallery as JSON.
- `POST /run` — `{ "source": "...", "input": "...", "target": "native" | "ti-basic" }` → `{ success, compile_errors, stdout, stderr, timed_out, truncated, tibasic, plots, duration_ms }`.
- `GET /play` — WebSocket for interactive sessions. The client sends `{ "source": "..." }` first, then `{ "stdin": "line\n" }` for each answer, or `{ "stop": true }` to end. The server sends `{ "event": ..., "data": ... }` messages: `compiling`, `compile_errors`, `error`, `started`, `stdout`, `stderr`, `truncated`, `timeout`, and finally `exit` (`"ok"` or `"stopped"`).

## Limits

Each run happens in its own temp directory that is deleted afterwards, with a 5 second run limit, 64 KB output cap, 64 KB source cap, and at most 2 programs running at once. Interactive Play sessions get their own limits: 10 minutes per game, 256 KB of streamed output, and at most 4 games at once.

**This server is for local use.** It binds to 127.0.0.1 only. Before exposing it publicly, put the runner in a real sandbox (gVisor/Firecracker), add rate limiting, and follow the Security section of `docs/playground-plan.md` — compiled programs are native code and can read files and open network connections.

## Configuration

| Env var | Default | Meaning |
|---------|---------|---------|
| `PORT` | `8080` | Port to listen on |
| `ENGLISHC` | `../target/release/englishc`, then `../target/debug/englishc` | Path to the compiler |
