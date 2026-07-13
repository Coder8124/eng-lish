use axum::extract::State;
use axum::extract::ws::{Message, WebSocket, WebSocketUpgrade};
use axum::http::StatusCode;
use axum::response::{Html, Response};
use axum::routing::{get, post};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;
use tokio::process::Command;
use tokio::sync::Semaphore;
use tokio::time::timeout;

const MAX_SOURCE_BYTES: usize = 64 * 1024;
const MAX_INPUT_BYTES: usize = 8 * 1024;
const MAX_OUTPUT_BYTES: usize = 64 * 1024;
const MAX_PLOT_BYTES: u64 = 512 * 1024;
const COMPILE_TIMEOUT: Duration = Duration::from_secs(20);
const RUN_TIMEOUT: Duration = Duration::from_secs(5);
const PLAY_TIMEOUT: Duration = Duration::from_secs(600);
const MAX_PLAY_OUTPUT_BYTES: usize = 256 * 1024;

struct AppState {
    repo_root: PathBuf,
    compiler: PathBuf,
    run_slots: Semaphore,
    play_slots: Semaphore,
    run_counter: AtomicU64,
}

#[derive(Deserialize)]
struct RunRequest {
    source: String,
    #[serde(default)]
    input: String,
    #[serde(default)]
    target: String,
}

#[derive(Serialize, Default)]
struct RunResponse {
    success: bool,
    compile_errors: String,
    stdout: String,
    stderr: String,
    timed_out: bool,
    truncated: bool,
    tibasic: String,
    plots: Vec<Plot>,
    duration_ms: u64,
}

#[derive(Serialize)]
struct Plot {
    name: String,
    html: String,
}

#[derive(Serialize)]
struct Example {
    name: String,
    title: String,
    description: String,
    input: String,
    source: String,
}

#[derive(Deserialize)]
struct ManifestEntry {
    file: String,
    title: String,
    description: String,
    #[serde(default)]
    input: String,
}

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf()
}

fn find_compiler(root: &Path) -> PathBuf {
    if let Ok(p) = std::env::var("ENGLISHC") {
        return PathBuf::from(p);
    }
    let release = root.join("target/release/englishc");
    if release.exists() {
        return release;
    }
    root.join("target/debug/englishc")
}

#[tokio::main]
async fn main() {
    let root = repo_root();
    let compiler = find_compiler(&root);
    if !compiler.exists() {
        eprintln!(
            "Compiler not found at {}. Build it first: cargo build",
            compiler.display()
        );
        std::process::exit(1);
    }

    let state = Arc::new(AppState {
        repo_root: root,
        compiler,
        run_slots: Semaphore::new(2),
        play_slots: Semaphore::new(4),
        run_counter: AtomicU64::new(0),
    });

    let app = Router::new()
        .route("/", get(index))
        .route("/examples", get(examples))
        .route("/run", post(run))
        .route("/play", get(play))
        .with_state(state);

    let port: u16 = std::env::var("PORT")
        .ok()
        .and_then(|p| p.parse().ok())
        .unwrap_or(8080);
    let listener = tokio::net::TcpListener::bind(("127.0.0.1", port))
        .await
        .unwrap();
    println!("eng-lish playground running at http://127.0.0.1:{port}");
    axum::serve(listener, app).await.unwrap();
}

async fn index() -> Html<&'static str> {
    Html(include_str!("../static/index.html"))
}

async fn examples(State(state): State<Arc<AppState>>) -> Json<Vec<Example>> {
    let manifest_path = state.repo_root.join("playground/examples.json");
    let entries: Vec<ManifestEntry> = std::fs::read_to_string(&manifest_path)
        .ok()
        .and_then(|s| serde_json::from_str(&s).ok())
        .unwrap_or_default();

    let mut out = Vec::new();
    for entry in entries {
        let path = state.repo_root.join("examples").join(&entry.file);
        if let Ok(source) = std::fs::read_to_string(&path) {
            out.push(Example {
                name: entry.file.trim_end_matches(".eng").to_string(),
                title: entry.title,
                description: entry.description,
                input: entry.input,
                source,
            });
        }
    }
    Json(out)
}

async fn run(
    State(state): State<Arc<AppState>>,
    Json(req): Json<RunRequest>,
) -> Result<Json<RunResponse>, (StatusCode, String)> {
    if req.source.len() > MAX_SOURCE_BYTES {
        return Err((
            StatusCode::PAYLOAD_TOO_LARGE,
            "Program is too big (64 KB max).".to_string(),
        ));
    }
    if req.input.len() > MAX_INPUT_BYTES {
        return Err((
            StatusCode::PAYLOAD_TOO_LARGE,
            "Program input is too big (8 KB max).".to_string(),
        ));
    }

    let _slot = state.run_slots.acquire().await.map_err(|_| {
        (
            StatusCode::SERVICE_UNAVAILABLE,
            "Server is shutting down.".to_string(),
        )
    })?;

    let id = state.run_counter.fetch_add(1, Ordering::Relaxed);
    let work_dir = std::env::temp_dir().join(format!(
        "eng-playground-{}-{}",
        std::process::id(),
        id
    ));
    let result = execute(&state, &req, &work_dir).await;
    let _ = tokio::fs::remove_dir_all(&work_dir).await;
    result.map(Json).map_err(|e| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            format!("Playground error: {e}"),
        )
    })
}

async fn setup_workdir(state: &AppState, source: &str, work_dir: &Path) -> Result<(), String> {
    tokio::fs::create_dir_all(work_dir)
        .await
        .map_err(|e| e.to_string())?;
    tokio::fs::write(work_dir.join("program.eng"), source)
        .await
        .map_err(|e| e.to_string())?;
    let _ = tokio::fs::symlink(state.repo_root.join("packages"), work_dir.join("packages")).await;
    Ok(())
}

fn compile_command(state: &AppState, work_dir: &Path, ti_basic: bool) -> Command {
    let mut cmd = Command::new(&state.compiler);
    cmd.arg("program.eng").current_dir(work_dir);
    if ti_basic {
        cmd.arg("--ti-basic");
    }
    cmd
}

async fn execute(
    state: &AppState,
    req: &RunRequest,
    work_dir: &Path,
) -> Result<RunResponse, String> {
    setup_workdir(state, &req.source, work_dir).await?;

    let started = std::time::Instant::now();
    let mut resp = RunResponse::default();

    let compile = capture(
        compile_command(state, work_dir, req.target == "ti-basic"),
        &[],
        COMPILE_TIMEOUT,
    )
    .await?;

    if compile.timed_out {
        resp.compile_errors = "Compiling took too long and was stopped.".to_string();
        resp.duration_ms = started.elapsed().as_millis() as u64;
        return Ok(resp);
    }
    if !compile.success {
        resp.compile_errors = if compile.stderr.trim().is_empty() {
            compile.stdout
        } else {
            compile.stderr
        };
        resp.duration_ms = started.elapsed().as_millis() as u64;
        return Ok(resp);
    }

    if req.target == "ti-basic" {
        resp.success = true;
        resp.tibasic = tokio::fs::read_to_string(work_dir.join("program.8xp.txt"))
            .await
            .unwrap_or_default();
        resp.duration_ms = started.elapsed().as_millis() as u64;
        return Ok(resp);
    }

    let mut run_cmd = Command::new(work_dir.join("program"));
    run_cmd.current_dir(work_dir);
    let run = capture(run_cmd, req.input.as_bytes(), RUN_TIMEOUT).await?;

    resp.success = run.success && !run.timed_out;
    resp.stdout = run.stdout;
    resp.stderr = run.stderr;
    resp.timed_out = run.timed_out;
    resp.truncated = run.truncated;
    resp.plots = collect_plots(work_dir).await;
    resp.duration_ms = started.elapsed().as_millis() as u64;
    Ok(resp)
}

async fn collect_plots(work_dir: &Path) -> Vec<Plot> {
    let mut plots = Vec::new();
    let Ok(mut entries) = tokio::fs::read_dir(work_dir).await else {
        return plots;
    };
    while let Ok(Some(entry)) = entries.next_entry().await {
        if plots.len() >= 4 {
            break;
        }
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("html") {
            continue;
        }
        if let Ok(meta) = entry.metadata().await
            && meta.len() <= MAX_PLOT_BYTES
            && let Ok(html) = tokio::fs::read_to_string(&path).await
        {
            plots.push(Plot {
                name: entry.file_name().to_string_lossy().to_string(),
                html,
            });
        }
    }
    plots.sort_by(|a, b| a.name.cmp(&b.name));
    plots
}

struct Captured {
    success: bool,
    timed_out: bool,
    truncated: bool,
    stdout: String,
    stderr: String,
}

async fn capture(
    mut cmd: Command,
    stdin_data: &[u8],
    wall: Duration,
) -> Result<Captured, String> {
    cmd.stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true);

    let mut child = cmd.spawn().map_err(|e| format!("failed to start: {e}"))?;

    let mut stdin = child.stdin.take().unwrap();
    let data = stdin_data.to_vec();
    tokio::spawn(async move {
        let _ = stdin.write_all(&data).await;
    });

    let stdout_task = read_capped(child.stdout.take().unwrap());
    let stderr_task = read_capped(child.stderr.take().unwrap());

    let mut timed_out = false;
    let status = match timeout(wall, child.wait()).await {
        Ok(status) => status.map_err(|e| e.to_string())?,
        Err(_) => {
            timed_out = true;
            let _ = child.kill().await;
            child.wait().await.map_err(|e| e.to_string())?
        }
    };

    let (stdout, out_trunc) = stdout_task.await;
    let (stderr, err_trunc) = stderr_task.await;

    Ok(Captured {
        success: status.success(),
        timed_out,
        truncated: out_trunc || err_trunc,
        stdout,
        stderr,
    })
}

#[derive(Deserialize)]
struct PlayClientMsg {
    #[serde(default)]
    source: Option<String>,
    #[serde(default)]
    stdin: Option<String>,
    #[serde(default)]
    stop: bool,
}

async fn play(ws: WebSocketUpgrade, State(state): State<Arc<AppState>>) -> Response {
    ws.on_upgrade(move |socket| play_session(socket, state))
}

async fn send_event(socket: &mut WebSocket, event: &str, data: &str) -> bool {
    let msg = serde_json::json!({ "event": event, "data": data }).to_string();
    socket.send(Message::Text(msg.into())).await.is_ok()
}

async fn play_session(mut socket: WebSocket, state: Arc<AppState>) {
    let Ok(_slot) = state.play_slots.try_acquire() else {
        let _ = send_event(
            &mut socket,
            "error",
            "Too many games are running right now — try again in a moment.",
        )
        .await;
        return;
    };

    let source = loop {
        match socket.recv().await {
            Some(Ok(Message::Text(text))) => {
                if let Ok(msg) = serde_json::from_str::<PlayClientMsg>(&text)
                    && let Some(source) = msg.source
                {
                    break source;
                }
            }
            Some(Ok(_)) => continue,
            _ => return,
        }
    };
    if source.len() > MAX_SOURCE_BYTES {
        let _ = send_event(&mut socket, "error", "Program is too big (64 KB max).").await;
        return;
    }

    let id = state.run_counter.fetch_add(1, Ordering::Relaxed);
    let work_dir =
        std::env::temp_dir().join(format!("eng-playground-{}-{}", std::process::id(), id));
    run_play_session(&mut socket, &state, &source, &work_dir).await;
    let _ = tokio::fs::remove_dir_all(&work_dir).await;
}

async fn run_play_session(
    socket: &mut WebSocket,
    state: &AppState,
    source: &str,
    work_dir: &Path,
) {
    if setup_workdir(state, source, work_dir).await.is_err() {
        let _ = send_event(socket, "error", "Could not set up the program.").await;
        return;
    }

    send_event(socket, "compiling", "").await;
    let compile = match capture(compile_command(state, work_dir, false), &[], COMPILE_TIMEOUT).await
    {
        Ok(c) => c,
        Err(e) => {
            let _ = send_event(socket, "error", &e).await;
            return;
        }
    };
    if !compile.success || compile.timed_out {
        let errors = if compile.stderr.trim().is_empty() {
            compile.stdout
        } else {
            compile.stderr
        };
        let _ = send_event(socket, "compile_errors", &errors).await;
        return;
    }

    let mut cmd = Command::new(work_dir.join("program"));
    cmd.current_dir(work_dir)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true);
    let mut child = match cmd.spawn() {
        Ok(c) => c,
        Err(_) => {
            let _ = send_event(socket, "error", "Could not start the program.").await;
            return;
        }
    };
    send_event(socket, "started", "").await;

    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();
    let mut out_buf = [0u8; 4096];
    let mut err_buf = [0u8; 4096];
    let mut stdout_done = false;
    let mut stderr_done = false;
    let mut exited: Option<bool> = None;
    let mut sent_bytes = 0usize;
    let mut timed_out = false;
    let deadline = tokio::time::sleep(PLAY_TIMEOUT);
    tokio::pin!(deadline);

    loop {
        tokio::select! {
            n = stdout.read(&mut out_buf), if !stdout_done => {
                match n {
                    Ok(0) | Err(_) => stdout_done = true,
                    Ok(n) => {
                        sent_bytes += n;
                        let text = String::from_utf8_lossy(&out_buf[..n]).to_string();
                        if !send_event(socket, "stdout", &text).await { break; }
                    }
                }
            }
            n = stderr.read(&mut err_buf), if !stderr_done => {
                match n {
                    Ok(0) | Err(_) => stderr_done = true,
                    Ok(n) => {
                        sent_bytes += n;
                        let text = String::from_utf8_lossy(&err_buf[..n]).to_string();
                        if !send_event(socket, "stderr", &text).await { break; }
                    }
                }
            }
            status = child.wait(), if exited.is_none() => {
                exited = Some(status.map(|s| s.success()).unwrap_or(false));
            }
            msg = socket.recv(), if exited.is_none() => {
                match msg {
                    Some(Ok(Message::Text(text))) => {
                        let Ok(parsed) = serde_json::from_str::<PlayClientMsg>(&text) else { continue };
                        if parsed.stop { break; }
                        if let Some(data) = parsed.stdin
                            && data.len() <= MAX_INPUT_BYTES
                            && stdin.write_all(data.as_bytes()).await.is_err()
                        {
                            continue;
                        }
                    }
                    Some(Ok(Message::Close(_))) | None => break,
                    _ => continue,
                }
            }
            _ = &mut deadline => {
                timed_out = true;
                break;
            }
        }

        if sent_bytes > MAX_PLAY_OUTPUT_BYTES {
            let _ = send_event(socket, "truncated", "").await;
            break;
        }
        if exited.is_some() && stdout_done && stderr_done {
            break;
        }
    }

    let _ = child.kill().await;
    if timed_out {
        let _ = send_event(socket, "timeout", "").await;
    }
    let _ = send_event(
        socket,
        "exit",
        if exited.unwrap_or(false) { "ok" } else { "stopped" },
    )
    .await;
}

async fn read_capped(reader: impl tokio::io::AsyncRead + Unpin) -> (String, bool) {
    let mut buf = Vec::new();
    let mut limited = reader.take(MAX_OUTPUT_BYTES as u64 + 1);
    let _ = limited.read_to_end(&mut buf).await;
    let truncated = buf.len() > MAX_OUTPUT_BYTES;
    buf.truncate(MAX_OUTPUT_BYTES);
    (String::from_utf8_lossy(&buf).to_string(), truncated)
}
