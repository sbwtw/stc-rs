use crate::ast::*;
use crate::parser::*;
use std::rc::Rc;

///
/// A  ->  Aα | β
///
/// A  ->  βA'
/// A' ->  αA' | ε
///

/// Parser functions result
type ParseResult<T> = Result<Option<T>, ParseError>;

/// declaration parser wrapper
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

/// function parser wrapper
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

/// LL(*) parser implementation
struct DefaultParserImpl<I: Iterator<Item = LexerResult>> {
    lexer: I,
    /// next token index
    next: usize,
    /// save all tokens, because we need to backtracking
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

    /// get next token from self.tokens[self.next], if self.next is not exist, get token from lexer.
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

    /// parse a function
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

    /// parse a declaration
    fn parse_declaration(&mut self) -> Result<Box<dyn Declaration>, ParseError> {
        todo!()
    }

    /// parse single statement
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

    /// expr': ":=" BitOrExpr expr' | ε
    fn parse_expression_fix(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;
        if !matches!(self.next()?.as_deref(), Some((_, Tok::Assign, _))) {
            self.next = pos;
            return Ok(None);
        }

        self.parse_bitor_expression()
        // if let Some(bitor) = self.parse_bitor_expression()? {
        //     let pos = self.next;
        //     let fix = self.parse_expression_fix()?;
        //     self.match_val(fix, pos, Some(bitor), |x, y| {
        //         Box::new(AssignExpression::new(x.unwrap(), y))
        //     })
        // } else {
        //     self.next = pos;
        //     Ok(None)
        // }
    }

    /// BitOrExpr: BitOrExpr "|" XorExpr
    ///      | XorExpr
    ///
    /// BitOrExpr: XorExpr bitor'
    /// bitor': "|" XorExpr bitor' | ε
    fn parse_bitor_expression(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        if let Some(bitor) = self.parse_xor_expression()? {
            if let Some(fix) = self.parse_bitor_expression_fix()? {
                Ok(Some(Box::new(OperatorExpression::new(
                    Tok::BitOr,
                    vec![bitor, fix],
                ))))
            } else {
                Ok(Some(bitor))
            }
        } else {
            self.next = pos;
            Ok(None)
        }
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
        let pos = self.next;

        match self.parse_bitand_expression()? {
            Some(bitand) => match self.parse_xor_expression_fix()? {
                Some(fix) => Ok(Some(Box::new(OperatorExpression::new(
                    Tok::Xor,
                    vec![bitand, fix],
                )))),
                None => Ok(Some(bitand)),
            },
            None => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    fn parse_xor_expression_fix(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;
        if !matches!(self.next()?.as_deref(), Some((_, Tok::Xor, _))) {
            self.next = pos;
            return Ok(None);
        }

        self.parse_xor_expression()
    }

    /// BitAndExpr: BitAndExpr "&" EquExpr
    ///        | EquExpr
    ///
    /// BitAndExpr: EquExpr bitandexpr'
    /// bitandexpr': "&" EquExpr bitandexpr' | ε
    fn parse_bitand_expression(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        match self.parse_equ_expr()? {
            Some(equ) => match self.parse_bitand_expression_fix()? {
                Some(fix) => Ok(Some(Box::new(OperatorExpression::new(
                    Tok::BitAnd,
                    vec![equ, fix],
                )))),
                None => Ok(Some(equ)),
            },
            None => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    fn parse_bitand_expression_fix(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;
        if !matches!(self.next()?.as_deref(), Some((_, Tok::BitAnd, _))) {
            self.next = pos;
            return Ok(None);
        }

        self.parse_bitand_expression()
    }

    /// EquExpr: EquExpr EquOp CmpExpr
    ///     | CmpExpr
    ///
    /// EquExpr: CmpExpr equexpr'
    fn parse_equ_expr(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        match self.parse_cmp_expr()? {
            Some(equ) => match self.parse_equ_expr_fix()? {
                Some((tok, fix)) => {
                    Ok(Some(Box::new(OperatorExpression::new(tok, vec![equ, fix]))))
                }
                None => Ok(Some(equ)),
            },
            None => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// equexpr': EquOp CmpExpr equexpr' | ε
    fn parse_equ_expr_fix(&mut self) -> ParseResult<(Tok, Box<dyn Expression>)> {
        let pos = self.next;

        let current_tok = match self.parse_equ_op()? {
            Some(tok) => tok,
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        let cmp_expr = match self.parse_cmp_expr()? {
            Some(cmp) => cmp,
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        let pos = self.next;
        match self.parse_equ_expr_fix()? {
            Some((tok, fix)) => {
                let tail = Box::new(OperatorExpression::new(tok, vec![cmp_expr, fix]));
                Ok(Some((current_tok, tail)))
            }
            _ => {
                self.next = pos;
                Ok(Some((current_tok, cmp_expr)))
            }
        }
    }

    fn parse_equ_op(&mut self) -> ParseResult<Tok> {
        let pos = self.next;

        match self.next()?.as_deref() {
            Some((_, tok @ Tok::Equal, _)) | Some((_, tok @ Tok::NotEqual, _)) => {
                Ok(Some(tok.clone()))
            }
            _ => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// CmpExpr: CmpExpr CmpOp OpExpr
    ///     | OpExpr
    ///
    /// CmpExpr: OpExpr cmpexpr'
    fn parse_cmp_expr(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        match self.parse_op_expr()? {
            Some(cmp) => match self.parse_cmp_expr_fix()? {
                Some((tok, fix)) => {
                    Ok(Some(Box::new(OperatorExpression::new(tok, vec![cmp, fix]))))
                }
                None => Ok(Some(cmp)),
            },
            None => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// cmpexpr': CmpOp OpExpr cmpexpr' | ε
    fn parse_cmp_expr_fix(&mut self) -> ParseResult<(Tok, Box<dyn Expression>)> {
        let pos = self.next;

        let current_tok = match self.parse_cmp_op()? {
            Some(tok) => tok,
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        let op_expr = match self.parse_op_expr()? {
            Some(cmp) => cmp,
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        let pos = self.next;
        match self.parse_cmp_expr_fix()? {
            Some((tok, fix)) => {
                let tail = Box::new(OperatorExpression::new(tok, vec![op_expr, fix]));
                Ok(Some((current_tok, tail)))
            }
            _ => {
                self.next = pos;
                Ok(Some((current_tok, op_expr)))
            }
        }
    }

    fn parse_cmp_op(&mut self) -> ParseResult<Tok> {
        let pos = self.next;

        match self.next()?.as_deref() {
            Some((_, tok @ Tok::Greater, _))
            | Some((_, tok @ Tok::GreaterEqual, _))
            | Some((_, tok @ Tok::Less, _))
            | Some((_, tok @ Tok::LessEqual, _)) => Ok(Some(tok.clone())),
            _ => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// OpExpr: OpExpr ExprOp Factor
    ///     | Factor
    ///
    /// OpExpr: Factor opexpr'
    fn parse_op_expr(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        if let Some(factor) = self.parse_factor()? {
            if let Some((tok, fix)) = self.parse_op_expr_fix()? {
                Ok(Some(Box::new(OperatorExpression::new(
                    tok,
                    vec![factor, fix],
                ))))
            } else {
                Ok(Some(factor))
            }
        } else {
            self.next = pos;
            Ok(None)
        }
    }

    /// opexpr': ExprOp Factor opexpr' | ε
    fn parse_op_expr_fix(&mut self) -> ParseResult<(Tok, Box<dyn Expression>)> {
        let pos = self.next;

        let current_tok = match self.parse_expr_op()? {
            Some(tok) => tok,
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        let factor = match self.parse_factor()? {
            Some(factor) => factor,
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        let pos = self.next;
        if let Some((tok, fix)) = self.parse_op_expr_fix()? {
            let tail = Box::new(OperatorExpression::new(tok, vec![factor, fix]));
            Ok(Some((current_tok, tail)))
        } else {
            self.next = pos;
            Ok(Some((current_tok, factor)))
        }
    }

    // + / -
    fn parse_expr_op(&mut self) -> ParseResult<Tok> {
        let pos = self.next;

        match self.next()?.as_deref() {
            Some((_, tok @ Tok::Plus, _)) | Some((_, tok @ Tok::Minus, _)) => Ok(Some(tok.clone())),
            _ => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// Factor: Factor FactorOp PowerExpr
    ///     | PowerExpr
    ///
    /// Factor: PowerExpr factor'
    fn parse_factor(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        if let Some(power) = self.parse_power_expr()? {
            if let Some((tok, fix)) = self.parse_factor_fix()? {
                Ok(Some(Box::new(OperatorExpression::new(
                    tok,
                    vec![power, fix],
                ))))
            } else {
                Ok(Some(power))
            }
        } else {
            self.next = pos;
            Ok(None)
        }
    }

    /// factor': FactorOp PowerExpr factor' | ε
    fn parse_factor_fix(&mut self) -> ParseResult<(Tok, Box<dyn Expression>)> {
        let pos = self.next;

        let current_tok = match self.parse_factor_op()? {
            Some(tok) => tok,
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        let power = match self.parse_power_expr()? {
            Some(power) => power,
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        let pos = self.next;
        if let Some((tok, fix)) = self.parse_factor_fix()? {
            let tail = Box::new(OperatorExpression::new(tok, vec![power, fix]));
            Ok(Some((current_tok, tail)))
        } else {
            self.next = pos;
            Ok(Some((current_tok, power)))
        }
    }

    // '*' '/' 'MOD'
    fn parse_factor_op(&mut self) -> ParseResult<Tok> {
        let pos = self.next;

        match self.next()?.as_deref() {
            Some((_, tok @ Tok::Multiply, _))
            | Some((_, tok @ Tok::Division, _))
            | Some((_, tok @ Tok::Mod, _)) => Ok(Some(tok.clone())),
            _ => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// PowerExpr: PowerExpr "**" UnaryFactor
    ///     | UnaryFactor
    ///
    /// PowerExpr: UnaryFactor powerexpr'
    fn parse_power_expr(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        match self.parse_unary_factor()? {
            Some(unary) => match self.parse_power_expr_fix()? {
                Some(fix) => Ok(Some(Box::new(OperatorExpression::new(
                    Tok::Power,
                    vec![unary, fix],
                )))),
                None => Ok(Some(unary)),
            },
            None => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    // powerexpr': "**" UnaryFactor powerexpr' | ε
    fn parse_power_expr_fix(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        if !matches!(self.next()?.as_deref(), Some((_, Tok::Power, _))) {
            self.next = pos;
            return Ok(None);
        }

        self.parse_power_expr()
    }

    /// UnaryFactor: UnaryOp CompoFactor
    ///     | CompoFactor
    fn parse_unary_factor(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;
        let unary_op = self.parse_unary_op()?;
        let compo_factor = self.parse_factor()?;

        match (unary_op, compo_factor) {
            (Some(op), Some(factor)) => {
                Ok(Some(Box::new(OperatorExpression::new(op, vec![factor]))))
            }
            (None, Some(factor)) => Ok(Some(factor)),
            _ => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    // '-' 'NOT'
    fn parse_unary_op(&mut self) -> ParseResult<Tok> {
        let pos = self.next;

        match self.next()?.as_deref() {
            Some((_, tok @ Tok::Not, _)) | Some((_, tok @ Tok::Minus, _)) => Ok(Some(tok.clone())),
            _ => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// CompoFactor: CompoFactor "." Term
    ///     | Term
    ///
    /// CompoFactor: Term compofactor'
    fn parse_compo_factor(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        match self.parse_term_expr()? {
            Some(term) => match self.parse_compo_factor_fix()? {
                Some(fix) => Ok(Some(Box::new(OperatorExpression::new(
                    Tok::Power,
                    vec![term, fix],
                )))),
                None => Ok(Some(term)),
            },
            None => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// compofactor': "." Term compofactor' | ε
    fn parse_compo_factor_fix(&mut self) -> ParseResult<Box<dyn Expression>> {
        let pos = self.next;

        if !matches!(self.next()?.as_deref(), Some((_, Tok::Access, _))) {
            self.next = pos;
            return Ok(None);
        }

        self.parse_power_expr()
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
