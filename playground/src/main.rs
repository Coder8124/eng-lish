use axum::extract::State;
use axum::http::StatusCode;
use axum::response::Html;
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

struct AppState {
    repo_root: PathBuf,
    compiler: PathBuf,
    run_slots: Semaphore,
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
        run_counter: AtomicU64::new(0),
    });

    let app = Router::new()
        .route("/", get(index))
        .route("/examples", get(examples))
        .route("/run", post(run))
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

async fn execute(
    state: &AppState,
    req: &RunRequest,
    work_dir: &Path,
) -> Result<RunResponse, String> {
    tokio::fs::create_dir_all(work_dir)
        .await
        .map_err(|e| e.to_string())?;
    tokio::fs::write(work_dir.join("program.eng"), &req.source)
        .await
        .map_err(|e| e.to_string())?;
    let _ = tokio::fs::symlink(state.repo_root.join("packages"), work_dir.join("packages")).await;

    let started = std::time::Instant::now();
    let mut resp = RunResponse::default();

    let mut compile_cmd = Command::new(&state.compiler);
    compile_cmd.arg("program.eng").current_dir(work_dir);
    if req.target == "ti-basic" {
        compile_cmd.arg("--ti-basic");
    }
    let compile = capture(compile_cmd, &[], COMPILE_TIMEOUT).await?;

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

async fn read_capped(reader: impl tokio::io::AsyncRead + Unpin) -> (String, bool) {
    let mut buf = Vec::new();
    let mut limited = reader.take(MAX_OUTPUT_BYTES as u64 + 1);
    let _ = limited.read_to_end(&mut buf).await;
    let truncated = buf.len() > MAX_OUTPUT_BYTES;
    buf.truncate(MAX_OUTPUT_BYTES);
    (String::from_utf8_lossy(&buf).to_string(), truncated)
}
