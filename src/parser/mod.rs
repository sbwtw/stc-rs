mod lexer;
pub use lexer::*;

mod token;

#[cfg(test)]
mod test;

pub use token::Tok;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(st, "/parser/st.rs");
pub use st::DeclarationParser;
pub use st::StFunctionParser;
