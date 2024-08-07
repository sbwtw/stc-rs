use crate::ast::*;
use crate::parser::token::Token;
use crate::parser::*;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(st, "/parser/lalrpop_impl/st.rs");

type LalrPopLexerItem = (usize, TokenKind, usize);
type LalrPopLexerResult = Result<LalrPopLexerItem, LexicalError>;

impl From<Token> for LalrPopLexerItem {
    fn from(tok: Token) -> Self {
        (tok.pos.mark, tok.kind, tok.pos.offset)
    }
}

struct LalrPopLexerWrapper<I: Iterator<Item = LexerResult>> {
    inner: I,
}

impl<I: Iterator<Item = LexerResult>> LalrPopLexerWrapper<I> {
    pub fn new(inner: I) -> Self {
        Self { inner }
    }
}

impl<I> Iterator for LalrPopLexerWrapper<I>
where
    I: Iterator<Item = LexerResult>,
{
    type Item = LalrPopLexerResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|x| x.map(Into::into))
    }
}

pub struct LalrpopDeclParser {
    inner: st::DeclarationParser,
}

impl DeclParserTrait for LalrpopDeclParser {
    fn parse_decl(&self, lexer: &mut StLexer) -> Result<Declaration, ParseError> {
        self.parse(lexer)
    }
}

impl LalrpopDeclParser {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn parse<I: IntoIterator<Item = LexerResult>>(
        &self,
        lexer: I,
    ) -> Result<Declaration, ParseError> {
        self.inner
            .parse(LalrPopLexerWrapper::new(lexer.into_iter()))
            .map_err(Into::into)
    }
}

impl Default for LalrpopDeclParser {
    fn default() -> Self {
        Self {
            inner: st::DeclarationParser::new(),
        }
    }
}

pub struct LalrpopParser {
    inner: st::StFunctionParser,
}

impl StmtParserTrait for LalrpopParser {
    fn parse_stmt(&self, lexer: &mut StLexer) -> Result<Statement, ParseError> {
        self.parse(lexer)
    }
}

impl LalrpopParser {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn parse<I: IntoIterator<Item = LexerResult>>(
        &self,
        lexer: I,
    ) -> Result<Statement, ParseError> {
        self.inner
            .parse(LalrPopLexerWrapper::new(lexer.into_iter()))
            .map_err(Into::into)
    }
}

impl Default for LalrpopParser {
    fn default() -> Self {
        Self {
            inner: st::StFunctionParser::new(),
        }
    }
}
