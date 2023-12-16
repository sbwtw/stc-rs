use crate::ast::*;
use crate::parser::*;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(st, "/parser/lalrpop_impl/st.rs");

type LalrPopLexerItem = (usize, Tok, usize);
type LalrPopLexerResult = Result<LalrPopLexerItem, LexicalError>;

impl From<Token> for LalrPopLexerItem {
    fn from(tok: Token) -> Self {
        (tok.pos.line, tok.tok, tok.pos.offset)
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

pub struct StDeclarationParser {
    inner: st::DeclarationParser,
}

impl StDeclarationParser {
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

impl Default for StDeclarationParser {
    fn default() -> Self {
        Self {
            inner: st::DeclarationParser::new(),
        }
    }
}

pub struct StFunctionParser {
    inner: st::StFunctionParser,
}

impl StFunctionParser {
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

impl Default for StFunctionParser {
    fn default() -> Self {
        Self {
            inner: st::StFunctionParser::new(),
        }
    }
}
