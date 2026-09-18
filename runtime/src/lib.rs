mod cluster;
mod list;
mod watched;

pub use cluster::englang_kMeans;

fn fail(message: &str) -> ! {
    eprintln!("Error: {}", message);
    std::process::exit(1);
}
