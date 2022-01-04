use crate::ast::*;
use crate::parser::*;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(st, "/parser/lalrpop_impl/st.rs");

pub struct StDeclarationParser {
    inner: st::DeclarationParser,
}

impl StDeclarationParser {
    pub fn new() -> Self {
        Self {
            inner: st::DeclarationParser::new(),
        }
    }

    pub fn parse<I: IntoIterator<Item = LexerResult>>(
        &self,
        lexer: I,
    ) -> Result<Declaration, ParseError> {
        self.inner.parse(lexer).map_err(Into::into)
    }
}

pub struct StFunctionParser {
    inner: st::StFunctionParser,
}

impl StFunctionParser {
    pub fn new() -> Self {
        Self {
            inner: st::StFunctionParser::new(),
        }
    }

    pub fn parse<I: IntoIterator<Item = LexerResult>>(
        &self,
        lexer: I,
    ) -> Result<Statement, ParseError> {
        self.inner.parse(lexer).map_err(Into::into)
    }
}
