use crate::ast::*;
use crate::parser::*;

pub struct StDeclarationParser {}

impl StDeclarationParser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse<I: IntoIterator<Item = lexer::LexerResult>>(
        &self,
        lexer: I,
    ) -> Result<Box<dyn Declaration>, ParseError> {
        DefaultParser::new(lexer).parse_declaration()
    }
}

pub struct StFunctionParser {}

impl StFunctionParser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse<I: IntoIterator<Item = lexer::LexerResult>>(
        &self,
        lexer: I,
    ) -> Result<Box<dyn Statement>, ParseError> {
        DefaultParser::new(lexer).parse_function()
    }
}

struct DefaultParser<I: IntoIterator<Item = LexerResult>> {
    lexer: I,
}

impl<I: IntoIterator<Item = LexerResult>> DefaultParser<I> {
    fn new(lexer: I) -> Self {
        Self { lexer }
    }

    fn parse_function(&mut self) -> Result<Box<dyn Statement>, ParseError> {
        todo!()
    }

    fn parse_declaration(&mut self) -> Result<Box<dyn Declaration>, ParseError> {
        todo!()
    }
}
