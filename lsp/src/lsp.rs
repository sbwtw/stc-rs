use crate::lsp_types::{TokenModifiers, TokenTypes};
use dashmap::DashMap;
use ropey::Rope;
use serde_json::Value;
use stc::parser::{StLexerBuilder, TokenKind};
use strum::IntoEnumIterator;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::*;

fn semantic_token_type_id(tok: &TokenKind) -> (u32, u32) {
    match tok {
        TokenKind::Identifier(_) => (TokenTypes::Variable as u32, TokenModifiers::None as u32),
        TokenKind::Literal(_) => (TokenTypes::Number as u32, TokenModifiers::None as u32),
        TokenKind::String => (TokenTypes::String as u32, TokenModifiers::None as u32),
        // operators
        op if op.is_operator() => (TokenTypes::Operator as u32, TokenModifiers::None as u32),
        // builtin-types
        TokenKind::Int => (TokenTypes::Type as u32, TokenModifiers::None as u32),
        // builtin-operators
        TokenKind::SizeOf | TokenKind::Adr => (
            TokenTypes::BuiltinFunction as u32,
            TokenModifiers::None as u32,
        ),
        // keywords
        TokenKind::If
        | TokenKind::Then
        | TokenKind::EndIf
        | TokenKind::Var
        | TokenKind::EndVar
        | TokenKind::Program
        | TokenKind::EndProgram => (TokenTypes::Keyword as u32, TokenModifiers::None as u32),
        _ => (TokenTypes::None as u32, TokenModifiers::None as u32),
    }
}

pub struct StcLsp {
    _client: Client,
    src_mgr: DashMap<Url, Rope>,
}

impl StcLsp {
    pub fn new(c: Client) -> Self {
        Self {
            _client: c,
            src_mgr: DashMap::new(),
        }
    }
}

impl StcLsp {
    pub fn on_file_change(&self, url: &Url, text: String) {
        let rope = text.into();
        self.src_mgr.insert(url.clone(), rope);
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for StcLsp {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        let capabilities = ServerCapabilities {
            // Semantic tokens highlighting
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                    legend: SemanticTokensLegend {
                        token_types: TokenTypes::iter().map(Into::into).collect(),
                        token_modifiers: TokenModifiers::iter().map(Into::into).collect(),
                    },
                    range: Some(false),
                    full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
                }),
            ),
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            document_highlight_provider: Some(OneOf::Left(true)),
            ..ServerCapabilities::default()
        };

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "stc-lsp".to_string(),
                version: Some("1.0".to_string()),
            }),
            capabilities,
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    // async fn initialized(&self, params: InitializedParams) {
    //     trace!("{:?}", params);
    // }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_file_change(&params.text_document.uri, params.text_document.text)
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        trace!("{:?}", params);

        for change in params.content_changes.into_iter() {
            // Only full text support
            assert!(change.range.is_none());
            assert!(change.range_length.is_none());

            self.on_file_change(&params.text_document.uri, change.text);
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        trace!("{:?}", params);
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        trace!("{:?}", params);

        debug_assert!(self.src_mgr.contains_key(&params.text_document.uri));
        self.src_mgr.remove(&params.text_document.uri);
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        trace!("{:?}", params.text_document_position_params);

        // let mut highlights = Vec::with_capacity(64);
        // highlights.push(DocumentHighlight {
        //     range: Range {
        //         start: Position {
        //             line: 0,
        //             character: 0,
        //         },
        //         end: Position {
        //             line: 0,
        //             character: 3,
        //         },
        //     },
        //     kind: None,
        // });

        // Ok(Some(highlights))

        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        trace!("{:?}", params);

        let s = self.src_mgr.get(&params.text_document.uri).unwrap();
        let lexer = StLexerBuilder::new().build_iter(s.chars());

        let mut last_line = 0;
        let mut last_offset = 0;
        let mut tokens: Vec<SemanticToken> = vec![];
        for tok in lexer.flatten() {
            if tok.pos.mark != last_line {
                last_offset = 0;
            }

            let (tt, tm) = semantic_token_type_id(&tok.kind);
            let token = SemanticToken {
                delta_line: (tok.pos.mark - last_line) as u32,
                delta_start: (tok.pos.offset - last_offset) as u32,
                length: tok.length as u32,
                token_type: tt,
                token_modifiers_bitset: tm,
            };
            tokens.push(token);

            last_line = tok.pos.mark;
            last_offset = tok.pos.offset;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    async fn semantic_tokens_full_delta(
        &self,
        params: SemanticTokensDeltaParams,
    ) -> Result<Option<SemanticTokensFullDeltaResult>> {
        trace!("{:?}", params);

        Ok(None)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        trace!("{:?}", params);

        // let range = params.range;
        // let lexer = StLexerBuilder::new()
        //     .build_file(params.text_document.uri.path())
        //     .map_err(|_| jsonrpc::Error::invalid_request())?;

        Ok(None)
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        trace!("{:?}", params);

        Ok(None)
    }
}
