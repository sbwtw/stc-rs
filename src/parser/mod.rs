use crate::ast::*;

mod lexer;
pub use lexer::*;

mod token;
pub use token::Tok;

#[cfg(test)]
mod test;

pub trait Lexer: Iterator {
    // fn next(&mut self) -> Result<(usize, Tok, usize), LexicalError>;
}

trait LalrpopLexer: Lexer {}

// impl Iterator for dyn Lexer {
//     type Item = (usize, Tok, usize);
//
//     fn next(&mut self) -> Option<Self::Item> {
//         match self.next() {
//             Ok(x) => Some(x),
//             Err(_) => None,
//         }
//     }
// }

pub trait Parser {
    type Result;

    fn parse<L: Lexer>(&mut self, lexer: L) -> Self::Result;
}

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(st, "/parser/st.rs");
pub use st::DeclarationParser;
// pub use st::StFunctionParser;

#[derive(Debug)]
pub struct ParseError {}

pub struct StFunctionParser {
    inner: st::StFunctionParser,
}

impl StFunctionParser {
    pub fn new() -> Self {
        Self {
            inner: st::StFunctionParser::new(),
        }
    }

    // pub fn parse<I: IntoIterator<Item = lexer::LexerResult>>(
    //     &self,
    //     lexer: I,
    // ) -> Result<Box<dyn Statement>, ParseError> {
    // }
}

impl Parser for StFunctionParser {
    type Result = Result<Box<dyn Statement>, ParseError>;

    fn parse<L: Lexer>(&mut self, lexer: L) -> Self::Result {
        match self.inner.parse(lexer) {
            Ok(r) => Ok(r),
            Err(e) => unimplemented!(),
        }
    }
}

// pub struct DeclarationParser {
//     inner: st::DeclarationParser,
// }
//
// impl DeclarationParser {
//     pub fn new() -> Self {
//         Self {
//             inner: st::DeclarationParser::new(),
//         }
//     }
//
//     pub fn parse<I: IntoIterator<Item = lexer::LexerResult>>(
//         &self,
//         lexer: I,
//     ) -> Result<Box<dyn Declaration>, ParseError> {
//         match self.inner.parse(lexer) {
//             Ok(r) => Ok(r),
//             Err(e) => ParseError(e.0, e.1, e.2),
//         }
//     }
// }
