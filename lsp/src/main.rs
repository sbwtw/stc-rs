mod lsp;

use lsp::StcLsp;

use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt().init();

    let read = tokio::io::stdin();
    let write = tokio::io::stdout();

    let (service, socket) = LspService::new(|c| StcLsp { client: c });
    Server::new(read, write, socket).serve(service).await;
}
