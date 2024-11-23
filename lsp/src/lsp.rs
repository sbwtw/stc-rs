use crate::lsp_types::{TokenModifiers, TokenTypes};
use dashmap::DashMap;
use ropey::Rope;
use serde_json::Value;
use stc::parser::{ParserBuilder, StLexerBuilder, TokenKind};
use stc::prelude::{ModuleContext, ModuleKind, UnitsManager, Uuid};
use strum::IntoEnumIterator;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::*;

const COMPLETION_TRIGGER_DOT: &str = ".";

fn semantic_token_type_id(tok: &TokenKind) -> (u32, u32) {
    match tok {
        TokenKind::Identifier(_) => (TokenTypes::Variable as u32, TokenModifiers::None as u32),
        TokenKind::Literal(_) => (TokenTypes::Number as u32, TokenModifiers::None as u32),
        TokenKind::String => (TokenTypes::String as u32, TokenModifiers::None as u32),
        // operators
        op if op.is_operator() => (TokenTypes::Operator as u32, TokenModifiers::None as u32),
        // builtin-operators
        TokenKind::SizeOf | TokenKind::Adr => (
            TokenTypes::BuiltinFunction as u32,
            TokenModifiers::None as u32,
        ),
        // builtin-types
        _ if tok.is_type() => (TokenTypes::Type as u32, TokenModifiers::None as u32),
        // keywords
        _ if tok.is_keywords() => (TokenTypes::Keyword as u32, TokenModifiers::None as u32),
        _ => (TokenTypes::None as u32, TokenModifiers::None as u32),
    }
}

pub struct StcLsp {
    client: Client,
    src_mgr: DashMap<Url, Rope>,
    uuid_mgr: DashMap<Url, Uuid>,
    _units_mgr: UnitsManager,
    app_ctx: ModuleContext,
}

impl StcLsp {
    pub fn new(c: Client) -> Self {
        let ctx = ModuleContext::new(ModuleKind::Application);
        let mgr = UnitsManager::new();
        mgr.write().add_context(ctx.clone());

        Self {
            client: c,
            src_mgr: DashMap::new(),
            _units_mgr: mgr,
            app_ctx: ctx,
            uuid_mgr: DashMap::new(),
        }
    }
}

impl StcLsp {
    // fn src_offset(&self, url: &Url, pos: Position) -> usize {
    //     let map = self.src_offset.get(url).unwrap();
    //     map.get(&(pos.line as usize)).unwrap() + pos.character as usize
    // }

    pub fn on_file_change(&self, url: &Url, range: Option<Range>, text: String) {
        if range.is_none() || !self.src_mgr.contains_key(url) {
            self.src_mgr.insert(url.clone(), text.into());
        } else {
            unimplemented!()
        }

        // let range = range.unwrap();
        // let start_offset = self.src_offset(url, range.start);
        // let end_offset = self.src_offset(url, range.end);
        //
        // let mut rope = self.src_mgr.get_mut(url).unwrap();
        // let rope = rope.value_mut();
        // rope.remove(start_offset..end_offset);
        // rope.insert(start_offset, &text);

        self.update_ast(url)
    }

    pub fn update_ast(&self, url: &Url) {
        let src_data = self.src_mgr.get(url).unwrap();
        let code = src_data.value();

        let mut lexer = StLexerBuilder::default().build_iter(code.chars());
        let parser = ParserBuilder::default().build();
        let uuid = self.uuid(url);

        // TODO: error records
        let (decl, body) = match parser.parse_pou(&mut lexer) {
            Ok((x, y)) => (x, y),
            Err(_) => return,
        };

        // Update ctx
        let mut ctx = self.app_ctx.write();
        let decl_id = ctx.add_declaration(decl, uuid);
        ctx.add_function(decl_id, body);
    }

    pub fn uuid(&self, url: &Url) -> Uuid {
        *self
            .uuid_mgr
            .entry(url.clone())
            .or_insert(Uuid::new_v4())
            .value()
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
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(false),
                trigger_characters: Some(vec![COMPLETION_TRIGGER_DOT.into()]),
                work_done_progress_options: Default::default(),
                all_commit_characters: None,
                completion_item: None,
            }),
            document_highlight_provider: Some(OneOf::Left(true)),
            // Use utf-8 for position encoding
            // position_encoding: Some(PositionEncodingKind::UTF8),
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
        self.client
            .show_message(MessageType::INFO, "shutdown")
            .await;

        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_file_change(&params.text_document.uri, None, params.text_document.text)
    }

    // async fn initialized(&self, params: InitializedParams) {
    //     trace!("{:?}", params);
    // }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        for change in params.content_changes.into_iter() {
            // range length is deprecated
            assert!(change.range_length.is_none());

            self.on_file_change(&params.text_document.uri, change.range, change.text);
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        trace!("did_save: {}", params.text_document.uri);

        if let Some(content) = params.text {
            self.on_file_change(&params.text_document.uri, None, content)
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        trace!("{:?}", params);

        debug_assert!(self.src_mgr.contains_key(&params.text_document.uri));
        self.src_mgr.remove(&params.text_document.uri);
    }

    async fn document_highlight(
        &self,
        _params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        // trace!("{:?}", params.text_document_position_params);

        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        trace!("tokens_full: {}", params.text_document.uri);

        let s = self.src_mgr.get(&params.text_document.uri).unwrap();
        let lexer = StLexerBuilder::new().build_iter(s.chars());

        let mut last_line = 0;
        let mut last_offset = 0;
        let mut tokens: Vec<SemanticToken> = vec![];
        for tok in lexer.flatten() {
            if tok.location.mark != last_line {
                last_offset = 0;
            }

            let (tt, tm) = semantic_token_type_id(&tok.kind);
            let token = SemanticToken {
                delta_line: (tok.location.mark - last_line) as u32,
                delta_start: (tok.location.offset - last_offset) as u32,
                length: tok.length as u32,
                token_type: tt,
                token_modifiers_bitset: tm,
            };
            tokens.push(token);

            last_line = tok.location.mark;
            last_offset = tok.location.offset;
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

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        if let Some(trigger) = params.context.and_then(|x| x.trigger_character) {
            // TODO: only handle '.' trigger
            if trigger != COMPLETION_TRIGGER_DOT {
                return Ok(None);
            }
        }

        let test_item = CompletionItem {
            label: String::from("test"),
            ..Default::default()
        };

        Ok(Some(CompletionResponse::Array(vec![test_item])))
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        trace!("{:?}", params);

        Ok(None)
    }
}
