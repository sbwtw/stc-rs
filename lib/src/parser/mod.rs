mod lexer;
pub use lexer::*;

mod token;
pub use token::Tok;

#[cfg(test)]
mod test;

#[derive(Clone, Debug)]
pub enum ParseError {
    LexerError(LexicalError),
    UnexpectedEnd,
    InvalidToken(usize),
    UnexpectedToken(usize, Vec<String>),
}

#[cfg(not(feature = "use_lalrpop"))]
impl ParseError {
    pub fn expect_tokens(pos: usize, tokens: &[Tok]) -> Self {
        let tokens: Vec<_> = tokens.iter().map(|x| x.into()).collect();

        Self::UnexpectedToken(pos, tokens)
    }
}

#[cfg(feature = "use_lalrpop")]
impl From<lalrpop_util::ParseError<usize, Tok, LexicalError>> for ParseError {
    fn from(e: lalrpop_util::ParseError<usize, Tok, LexicalError>) -> Self {
        match e {
            lalrpop_util::ParseError::InvalidToken { location: loc } => {
                ParseError::InvalidToken(loc)
            }
            lalrpop_util::ParseError::UnrecognizedEOF {
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

#[cfg(feature = "use_lalrpop")]
mod lalrpop_impl;
#[cfg(feature = "use_lalrpop")]
pub use lalrpop_impl::{StDeclarationParser, StFunctionParser};

#[cfg(not(feature = "use_lalrpop"))]
mod default_impl;
#[cfg(not(feature = "use_lalrpop"))]
pub use default_impl::{StDeclarationParser, StFunctionParser};
