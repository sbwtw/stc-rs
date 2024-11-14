use crate::prelude::*;

mod buffer;

pub use buffer::*;

mod ststring;
pub use ststring::StString;

mod lexer;
pub use lexer::*;

mod operator;
pub use operator::Operator;

mod token;
pub use token::TokenKind;

#[macro_export]
macro_rules! parse_statement {
    ($code: literal) => {{
        let mut lexer = StLexerBuilder::new().build_str($code);
        let parser = ParserBuilder::default().build();
        parser.parse_stmt(&mut lexer)
    }};
}

#[cfg(test)]
mod test;

#[derive(Clone, Debug)]
pub enum ParseError {
    LexerError(LexicalError),
    UnexpectedEnd,
    InvalidToken(usize),
    InvalidTokenAt(String),
    UnexpectedToken(usize, Vec<String>),
}

impl ParseError {
    pub fn expect_tokens(pos: usize, tokens: &[TokenKind]) -> Self {
        let tokens: Vec<_> = tokens.iter().map(|x| x.into()).collect();

        Self::UnexpectedToken(pos, tokens)
    }
}

#[cfg(feature = "lalrpop_parser")]
impl From<lalrpop_util::ParseError<usize, TokenKind, LexicalError>> for ParseError {
    fn from(e: lalrpop_util::ParseError<usize, TokenKind, LexicalError>) -> Self {
        match e {
            lalrpop_util::ParseError::InvalidToken { location: loc } => {
                ParseError::InvalidToken(loc)
            }
            lalrpop_util::ParseError::UnrecognizedEof {
                location: _,
                expected: _,
            } => ParseError::UnexpectedEnd,
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (loc, _, _),
                expected: exp,
            } => ParseError::UnexpectedToken(loc, exp),
            lalrpop_util::ParseError::ExtraToken { token: (loc, _, _) } => {
                ParseError::InvalidToken(loc)
            }
            lalrpop_util::ParseError::User { error: e } => ParseError::LexerError(e),
        }
    }
}

pub struct Parser<T: ParserTrait> {
    inner: T,
}

impl<T: ParserTrait> Parser<T> {
    #[inline]
    pub fn name(&self) -> String {
        self.inner.name()
    }

    #[inline]
    pub fn parse_pou(&self, lexer: &mut StLexer) -> Result<(Declaration, Statement), ParseError> {
        self.inner.parse_pou(lexer)
    }

    #[inline]
    pub fn parse_decl(&self, lexer: &mut StLexer) -> Result<Declaration, ParseError> {
        self.inner.parse_decl(lexer)
    }

    #[inline]
    pub fn parse_stmt(&self, lexer: &mut StLexer) -> Result<Statement, ParseError> {
        self.inner.parse_stmt(lexer)
    }

    #[inline]
    pub fn parse_literal_from_str<S: AsRef<str>>(
        &self,
        s: S,
    ) -> Result<LiteralExpression, ParseError> {
        let lexer = StLexerBuilder::new().build_str(s.as_ref());
        self.parse_literal(lexer)
    }

    #[inline]
    pub fn parse_literal(&self, mut lexer: StLexer) -> Result<LiteralExpression, ParseError> {
        self.inner.parse_literal(&mut lexer)
    }
}

#[derive(Default)]
pub struct ParserBuilder {}

impl ParserBuilder {
    #[cfg(feature = "lalrpop_parser")]
    pub fn build(self) -> Parser<LalrpopParser> {
        Parser {
            inner: LalrpopParser::new(),
        }
    }

    #[cfg(not(feature = "lalrpop_parser"))]
    pub fn build(self) -> Parser<DefaultParser> {
        Parser {
            inner: DefaultParser::new(),
        }
    }
}

pub trait ParserTrait {
    fn name(&self) -> String;
    fn parse_pou(&self, lexer: &mut StLexer) -> Result<(Declaration, Statement), ParseError>;
    fn parse_decl(&self, lexer: &mut StLexer) -> Result<Declaration, ParseError>;
    fn parse_stmt(&self, lexer: &mut StLexer) -> Result<Statement, ParseError>;
    fn parse_literal(&self, lexer: &mut StLexer) -> Result<LiteralExpression, ParseError>;
    fn parse_expression(&self, lexer: &mut StLexer) -> Result<Expression, ParseError>;
}

#[cfg(feature = "lalrpop_parser")]
mod lalrpop_impl;
#[cfg(feature = "lalrpop_parser")]
use lalrpop_impl::LalrpopParser;

#[cfg(not(feature = "lalrpop_parser"))]
mod default_impl;
#[cfg(not(feature = "lalrpop_parser"))]
use default_impl::DefaultParser;
