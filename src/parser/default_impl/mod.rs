use crate::ast::*;
use crate::parser::*;
use std::rc::Rc;

///
/// A  ->  Aα | β
///
/// A  ->  βA'
/// A' ->  αA' | ε
///

macro_rules! match_all {
    ($self: ident, $($fun:ident),*) => {
        {
            let mut temp_vec = Vec::new();
            let mut brk = false;
            $(
                if !brk {
                    let pos = $self.next;
                    match $self.$fun()? {
                        Some(val) => temp_vec.push(($self.next, Some(val))),
                        _ => {
                            $self.next = pos;
                            brk = true;
                        },
                    }
                }
            )*

            temp_vec
        }
    };
}

macro_rules! match_binary_op {
    ($self: ident, $tok: expr, $($fun: ident), *) => {
        let mut v = match_all!($self, $($fun),*);
        if v.len() == 2 {
            let v0 = v[0].1.take().unwrap();
            let v1 = v[1].1.take().unwrap();
            return Ok(Some(Box::new(OperatorExpression::new(
                $tok,
                vec![v0, v1],
            ))));
        }

        if v.len() == 1 {
            $self.next = v[0].0;
            let v = v[0].1.take().unwrap();
            return Ok(Some(v));
        }

        return Ok(None);
    }
}

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

    fn match_val<T, F: FnOnce(Option<T>, T) -> T>(
        &mut self,
        val: Option<T>,
        fallback_pos: usize,
        fallback_value: Option<T>,
        success_factor: F,
    ) -> ParseResult<T> {
        match val {
            Some(v) => Ok(Some(success_factor(fallback_value, v))),
            _ => {
                self.next = fallback_pos;
                Ok(fallback_value)
            }
        }
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

        if !matches!(self.next()?.as_deref(), Some((_, Tok::Semicolon, _))) {
            self.next = pos;
            return Ok(None);
        }

        Ok(Some(Box::new(ExprStatement::new(expr))))
    }

    fn parse_op_expression(&mut self) -> ParseResult<Box<dyn Expression>> {
        todo!()
    }

    /// Expr: Expr ":=" BitOrExpr
    ///     | BitOrExpr  
    ///
    /// Expr: BitOrExpr expr'
    /// expr': ":=" BitOrExpr expr' | ε
    fn parse_expression(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;
        if let Some(bitor) = self.parse_bitor_expression()? {
            return match self.parse_expression_fix()? {
                Some(fix) => Ok(Some(Box::new(AssignExpression::new(bitor, fix)))),
                None => {
                    self.next = pos;
                    Ok(Some(bitor))
                }
            };
        }

        Ok(None)
    }

    fn parse_expression_fix(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;
        if !matches!(self.next()?.as_deref(), Some((_, Tok::Assign, _))) {
            self.next = pos;
            return Ok(None);
        }

        if let Some(bitor) = self.parse_bitor_expression()? {
            let pos = self.next;
            let fix = self.parse_expression_fix()?;
            self.match_val(fix, pos, Some(bitor), |x, y| {
                Box::new(AssignExpression::new(x.unwrap(), y))
            })
        } else {
            self.next = pos;
            Ok(None)
        }
    }

    /// BitOrExpr: BitOrExpr "|" XorExpr
    ///      | XorExpr
    ///
    /// BitOrExpr: XorExpr bitor'
    /// bitor': "|" XorExpr bitor' | ε
    fn parse_bitor_expression(&mut self) -> ParseResult<Box<dyn Expression>> {
        match_binary_op!(
            self,
            Tok::BitOr,
            parse_xor_expression,
            parse_bitor_expression_fix
        );
    }

    fn parse_bitor_expression_fix(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;
        if !matches!(self.next()?.as_deref(), Some((_, Tok::BitOr, _))) {
            self.next = pos;
            return Ok(None);
        }

        self.parse_bitor_expression()
    }

    /// XorExpr: XorExpr "XOR" BitAndExpr
    ///        | BitAndExpr
    ///
    /// XorExpr: BitAndExpr xorexpr'
    /// xorexpr': "XOR" BitAndExpr xorexpr' | ε
    fn parse_xor_expression(&mut self) -> ParseResult<Box<dyn Expression>> {
        let mut v = match_all!(self, parse_bitand_expression, parse_xor_expression_fix);
        if v.len() == 2 {
            let v0 = v[0].1.take().unwrap();
            let v1 = v[1].1.take().unwrap();
            return Ok(Some(Box::new(OperatorExpression::new(
                Tok::Xor,
                vec![v0, v1],
            ))));
        }

        if v.len() == 1 {
            self.next = v[0].0;
            let v = v[0].1.take().unwrap();
            return Ok(Some(v));
        }

        Ok(None)
    }

    fn parse_xor_expression_fix(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;
        if !matches!(self.next()?.as_deref(), Some((_, Tok::Xor, _))) {
            self.next = pos;
            return Ok(None);
        }

        self.parse_xor_expression()
    }

    fn parse_bitand_expression(&mut self) -> ParseResult<Box<dyn Expression>> {
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
