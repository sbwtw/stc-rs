use crate::ast::*;
use crate::parser::*;
use std::rc::Rc;

type ParseResult<T> = Result<Option<T>, ParseError>;

pub struct StDeclarationParser {}

impl StDeclarationParser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse<I>(&self, lexer: I) -> Result<Box<dyn Declaration>, ParseError>
    where
        I: IntoIterator<Item = LexerResult>,
    {
        DefaultParserImpl::new(lexer.into_iter()).parse_declaration()
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
        DefaultParserImpl::new(lexer.into_iter()).parse_function()
    }
}

struct DefaultParserImpl<I: Iterator<Item = LexerResult>> {
    lexer: I,
    next: usize,
    tokens: Vec<Rc<LexerItem>>,
}

impl<I: Iterator<Item = LexerResult>> DefaultParserImpl<I> {
    fn new(lexer: I) -> Self {
        Self {
            lexer,
            next: 0,
            tokens: vec![],
        }
    }

    fn next(&mut self) -> Result<Option<Rc<LexerItem>>, ParseError> {
        while self.next >= self.tokens.len() {
            match self.lexer.next() {
                Some(Ok(item)) => self.tokens.push(Rc::new(item)),
                Some(Err(e)) => return Err(ParseError::LexerError(e)),
                None => return Ok(None),
            }
        }

        let r = Ok(Some(self.tokens[self.next].clone()));
        self.next += 1;

        r
    }

    fn parse_function(&mut self) -> Result<Box<dyn Statement>, ParseError> {
        let mut statements: Vec<_> = vec![];

        loop {
            match self.parse_statement() {
                Ok(Some(stmt)) => statements.push(stmt),
                Ok(None) => break,
                Err(e) => return Err(e),
            }
        }

        match self.next()? {
            None => {}
            _ => return Err(ParseError::InvalidToken(self.next)),
        }

        Ok(Box::new(StatementList::new(statements)))
    }

    fn parse_declaration(&mut self) -> Result<Box<dyn Declaration>, ParseError> {
        todo!()
    }

    fn parse_statement(&mut self) -> ParseResult<Box<dyn Statement>> {
        self.parse_expr_statement()
    }

    fn parse_expr_statement(&mut self) -> ParseResult<Box<dyn Statement>> {
        let pos = self.next;
        let expr = match self.parse_expression()? {
            Some(expr) => expr,
            None => {
                self.next = pos;
                return Ok(None);
            }
        };

        match self.next()?.as_deref() {
            Some((_, Tok::Semicolon, _)) => {}
            _ => {
                self.next = pos;
                return Ok(None);
            }
        }

        Ok(Some(Box::new(ExprStatement::new(expr))))
    }

    fn parse_expression(&mut self) -> ParseResult<Box<dyn Expression>> {
        if let Some(r) = self.parse_assign_expression()? {
            return Ok(Some(r));
        }

        if let Some(r) = self.parse_bitor_expression()? {
            return Ok(Some(r));
        }

        Ok(None)
    }

    fn parse_op_expression(&mut self) -> ParseResult<Box<dyn Expression>> {
        todo!()
    }

    fn parse_assign_expression(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        let lhs = match self.parse_variable_expr()? {
            Some(var) => var,
            None => {
                self.next = pos;
                return Ok(None);
            }
        };

        match self.next()?.as_deref() {
            Some((_, Tok::Assign, _)) => {}
            _ => {
                self.next = pos;
                return Ok(None);
            }
        }

        let rhs = match self.parse_expression()? {
            Some(expr) => expr,
            None => {
                self.next = pos;
                return Ok(None);
            }
        };

        Ok(Some(Box::new(AssignExpression::new(lhs, rhs))))
    }

    fn parse_bitor_expression(&mut self) -> ParseResult<Box<dyn Expression>> {
        self.parse_term_expr()
    }

    fn parse_term_expr(&mut self) -> ParseResult<Box<dyn Expression>> {
        if let Some(val) = self.parse_variable_expr()? {
            return Ok(Some(val));
        }

        if let Some(literal) = self.parse_literal_expr()? {
            return Ok(Some(literal));
        }

        Ok(None)
    }

    fn parse_literal_expr(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        match self.next()?.as_deref() {
            Some((_, Tok::Literal(val), _)) => {
                return Ok(Some(Box::new(LiteralExpression::new(val.clone()))))
            }
            _ => {}
        }

        self.next = pos;
        Ok(None)
    }

    fn parse_variable_expr(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        let var = match self.next()?.as_deref() {
            Some((_, Tok::Identifier(ident), _)) => Variable::new(ident.clone()),
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        Ok(Some(Box::new(var)))
    }
}
