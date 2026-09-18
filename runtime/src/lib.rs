mod cluster;
mod list;

pub use cluster::englang_kMeans;

fn fail(message: &str) -> ! {
    eprintln!("Error: {}", message);
    std::process::exit(1);
}
