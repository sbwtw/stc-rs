use crate::prelude::{Declaration, Statement};

use once_cell::sync::Lazy;

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

pub struct Parser {
    decl_parser: Lazy<Box<dyn DeclParserTrait>>,
    stmt_parser: Lazy<Box<dyn StmtParserTrait>>,
}

impl Parser {
    pub fn parse(&self, mut lexer: StLexer) -> Result<Declaration, ParseError> {
        self.decl_parser.parse_decl(&mut lexer)
    }

    pub fn parse_stmt(&self, mut lexer: StLexer) -> Result<Statement, ParseError> {
        self.stmt_parser.parse_stmt(&mut lexer)
    }

    pub fn parse_literal<S: AsRef<str>>(&self, s: S) -> LiteralValue {
        todo!()
    }
}

#[derive(Default)]
pub struct ParserBuilder {}

impl ParserBuilder {
    #[cfg(feature = "lalrpop_parser")]
    pub fn build(self) -> Parser {
        Parser {
            decl_parser: Lazy::new(|| Box::new(LalrpopDeclParser::new())),
            stmt_parser: Lazy::new(|| Box::new(LalrpopParser::new())),
        }
    }

    #[cfg(feature = "default_parser")]
    pub fn build(self) -> Parser {
        Parser {
            decl_parser: Lazy::new(|| Box::new(DefaultDeclParser::new())),
            stmt_parser: Lazy::new(|| Box::new(DefaultStmtParser::new())),
        }
    }
}

trait DeclParserTrait {
    fn parse_decl(&self, lexer: &mut StLexer) -> Result<Declaration, ParseError>;
}

trait StmtParserTrait {
    fn parse_stmt(&self, lexer: &mut StLexer) -> Result<Statement, ParseError>;
}

#[cfg(feature = "lalrpop_parser")]
mod lalrpop_impl;
#[cfg(feature = "lalrpop_parser")]
use lalrpop_impl::{LalrpopDeclParser, LalrpopParser};

#[cfg(feature = "default_parser")]
mod default_impl;
#[cfg(feature = "default_parser")]
use default_impl::{DefaultDeclParser, DefaultStmtParser};

#[cfg(all(feature = "default_parser", feature = "lalrpop_parser"))]
compile_error!("Feature default_parser and lalrpop_parser are mutually exclusive and cannot be enabled together");
