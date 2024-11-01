mod lsp;
mod lsp_types;

use clap::Parser;
use lsp::StcLsp;
use std::io;
use tokio::net::TcpSocket;
use tower_lsp::{LspService, Server};
use tracing::info;

#[derive(Parser)]
struct LSPServerArgs {
    #[arg(long)]
    stdio: bool,

    #[arg(long)]
    port: Option<u16>,
}

#[tokio::main]
async fn main() -> io::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter("lsp=trace,debug")
        .with_level(true)
        .with_writer(io::stderr)
        .with_ansi(false)
        .init();

    let args = LSPServerArgs::parse();
    info!(
        "Starting lsp server with stdio = {}, port = {:?}",
        args.stdio, args.port
    );

    if args.stdio {
        let (rx, tx) = (tokio::io::stdin(), tokio::io::stdout());
        let (service, socket) = LspService::new(StcLsp::new);

        Server::new(rx, tx, socket).serve(service).await;
        Ok(())
    } else {
        let addr = format!("127.0.0.1:{}", args.port.expect("Port must be specified"))
            .parse()
            .unwrap();
        let socket = TcpSocket::new_v4()?;
        socket.bind(addr)?;

        let listener = socket.listen(1024)?;
        while let Ok((mut stream, _addr)) = listener.accept().await {
            let (rx, tx) = stream.split();
            let (service, socket) = LspService::new(StcLsp::new);

            Server::new(rx, tx, socket).serve(service).await;
        }

        Ok(())
    }
}
