use crate::ast::*;
use crate::parser::token::Token;
use crate::parser::*;
use lalrpop_util::lalrpop_mod;
use once_cell::sync::Lazy;

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

pub struct LalrpopParser {
    decl_parser: Lazy<st::DeclarationParser>,
    body_parser: Lazy<st::StFunctionParser>,
}

impl LalrpopParser {
    pub fn new() -> Self {
        Self {
            decl_parser: Lazy::new(st::DeclarationParser::new),
            body_parser: Lazy::new(st::StFunctionParser::new),
        }
    }
}

impl ParserTrait for LalrpopParser {
    #[inline]
    fn parse_decl(&self, lexer: &mut StLexer) -> Result<Declaration, ParseError> {
        self.decl_parser
            .parse(LalrPopLexerWrapper::new(lexer.into_iter()))
            .map_err(Into::into)
    }

    #[inline]
    fn parse_stmt(&self, lexer: &mut StLexer) -> Result<Statement, ParseError> {
        self.body_parser
            .parse(LalrPopLexerWrapper::new(lexer.into_iter()))
            .map_err(Into::into)
    }

    #[inline]
    fn parse_literal(&self, lexer: &mut StLexer) -> Result<LiteralExpression, ParseError> {
        todo!()
    }
}
