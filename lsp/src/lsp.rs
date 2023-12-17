use serde_json::Value;
use stc::parser::{StLexerBuilder, Tok};
use tower_lsp::jsonrpc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::*;

const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::new("none"),
    // 1, STATIC
    SemanticTokenModifier::STATIC,
    // 2, GLOBAL
    SemanticTokenModifier::new("global"),
    // 4, RETAIN
    SemanticTokenModifier::new("retain"),
];

const TOKEN_TYPES: &[SemanticTokenType] = &[
    // 0, NONE
    SemanticTokenType::new("none"),
    // 1, keywords
    SemanticTokenType::KEYWORD,
    // 2, identifiers
    SemanticTokenType::VARIABLE,
    // 3, operators
    SemanticTokenType::OPERATOR,
    // 4, builtin functions
    SemanticTokenType::new("builtin-functions"),
    // 5, number literals
    SemanticTokenType::NUMBER,
    // 6, string literals
    SemanticTokenType::STRING,
    // 7, type
    SemanticTokenType::TYPE,
];

fn semantic_token_type_id(tok: &Tok) -> (u32, u32) {
    match tok {
        Tok::Identifier(_) => (2, 0),
        Tok::Literal(_) => (5, 0),
        Tok::String => (6, 0),
        // operators
        op if op.is_operator() => (3, 0),
        // builtin-types
        Tok::Int => (7, 0),
        // keywords
        Tok::If
        | Tok::Then
        | Tok::EndIf
        | Tok::Var
        | Tok::EndVar
        | Tok::Program
        | Tok::EndProgram => (1, 0),
        _ => (0, 0),
    }
}

#[derive(Debug)]
pub struct StcLsp {
    pub client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for StcLsp {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        trace!("initialize");

        let capabilities = ServerCapabilities {
            // Semantic tokens highlighting
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                    legend: SemanticTokensLegend {
                        token_types: TOKEN_TYPES.to_vec(),
                        token_modifiers: TOKEN_MODIFIERS.to_vec(),
                    },
                    range: Some(true),
                    full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
                }),
            ),
            document_highlight_provider: Some(OneOf::Left(true)),
            ..ServerCapabilities::default()
        };

        Ok(InitializeResult {
            server_info: None,
            capabilities,
        })
    }

    async fn initialized(&self, params: InitializedParams) {
        trace!("{:?}", params);
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        trace!("{:?}", params);
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

        let lexer = StLexerBuilder::new()
            .build_file(params.text_document.uri.path())
            .map_err(|_| jsonrpc::Error::invalid_request())?;

        let mut last_line = 0;
        let mut last_offset = 0;
        let mut tokens: Vec<SemanticToken> = vec![];
        for tok in lexer.flatten() {
            if tok.pos.line != last_line {
                last_offset = 0;
            }

            let (tt, tm) = semantic_token_type_id(&tok.tok);
            let token = SemanticToken {
                delta_line: (tok.pos.line - last_line) as u32,
                delta_start: (tok.pos.offset - last_offset) as u32,
                length: tok.length as u32,
                token_type: tt,
                token_modifiers_bitset: tm,
            };
            tokens.push(token);

            last_line = tok.pos.line;
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

        Ok(None)
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        trace!("{:?}", params);

        Ok(None)
    }
}
