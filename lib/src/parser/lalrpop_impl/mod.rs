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
    body_parser: Lazy<st::StBodyParser>,
    pou_parser: Lazy<st::StPOUParser>,
    literal_parser: Lazy<st::LiteralExprParser>,
    expression_parser: Lazy<st::ExprParser>,
}

impl LalrpopParser {
    pub fn new() -> Self {
        Default::default()
    }
}

impl Default for LalrpopParser {
    fn default() -> Self {
        Self {
            decl_parser: Lazy::new(st::DeclarationParser::new),
            body_parser: Lazy::new(st::StBodyParser::new),
            pou_parser: Lazy::new(st::StPOUParser::new),
            literal_parser: Lazy::new(st::LiteralExprParser::new),
            expression_parser: Lazy::new(st::ExprParser::new),
        }
    }
}

impl ParserTrait for LalrpopParser {
    #[inline]
    fn name(&self) -> String {
        "Lalrpop Parser".to_string()
    }

    #[inline]
    fn parse_pou(&self, lexer: &mut StLexer) -> Result<(Declaration, Statement), ParseError> {
        self.pou_parser
            .parse(LalrPopLexerWrapper::new(lexer.into_iter()))
            .map_err(Into::into)
    }

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
        self.literal_parser
            .parse(LalrPopLexerWrapper::new(lexer.into_iter()))
            .map_err(Into::into)
    }

    #[inline]
    fn parse_expression(&self, lexer: &mut StLexer) -> Result<Expression, ParseError> {
        self.expression_parser
            .parse(LalrPopLexerWrapper::new(lexer.into_iter()))
            .map_err(Into::into)
    }
}
