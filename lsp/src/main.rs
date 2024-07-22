mod lsp;
mod lsp_types;

use lsp::StcLsp;
use std::io;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter("lsp=trace,debug")
        .with_level(true)
        .with_writer(io::stderr)
        .with_ansi(false)
        .init();

    let read = tokio::io::stdin();
    let write = tokio::io::stdout();

    let (service, socket) = LspService::new(StcLsp::new);
    Server::new(read, write, socket).serve(service).await;
}
