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
    #[allow(dead_code)]
    UnexpectedEnd,
    InvalidToken(usize),
    UnexpectedToken(usize, Vec<String>),
}

impl ParseError {
    pub fn expect_tokens(pos: usize, tokens: &[TokenKind]) -> Self {
        let tokens: Vec<_> = tokens.iter().map(|x| x.into()).collect();

        Self::UnexpectedToken(pos, tokens)
    }
}

#[cfg(feature = "use_lalrpop")]
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
    inner: Box<dyn ParserTrait>,
}

pub struct ParserBuilder {}

impl ParserBuilder {
    #[cfg(feature = "use_lalrpop")]
    pub fn build(self) -> Parser {
        Parser {
            inner: Box::new(LalrpopDeclParser::new()),
        }
    }

    #[cfg(not(feature = "use_lalrpop"))]
    pub fn build(self) -> Parser {
        Parser {
            inner: Box::new(StDeclarationParser::new()),
        }
    }
}

trait ParserTrait {}

#[cfg(feature = "use_lalrpop")]
mod lalrpop_impl;
#[cfg(feature = "use_lalrpop")]
pub use lalrpop_impl::{LalrpopDeclParser, LalrpopParser};

mod default_impl;
pub use default_impl::{StDeclarationParser, StFunctionParser};
