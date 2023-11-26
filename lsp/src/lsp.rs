use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::*;

#[derive(Debug)]
pub struct StcLsp {
    pub client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for StcLsp {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        debug!("initialize");

        let capabilities = ServerCapabilities {
            inlay_hint_provider: Some(OneOf::Left(true)),
            ..ServerCapabilities::default()
        };

        Ok(InitializeResult {
            server_info: None,
            capabilities,
        })
    }

    async fn initialized(&self, params: InitializedParams) {
        dbg!(params);
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        dbg!(params);
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        dbg!(params);
        Ok(None)
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        dbg!(params);

        Ok(None)
    }
}
