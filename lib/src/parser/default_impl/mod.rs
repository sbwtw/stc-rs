use crate::ast::*;
use crate::parser::*;
use smallvec::smallvec;
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
#[allow(unused)]
pub struct StDeclarationParser {}

impl StDeclarationParser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse<I>(&self, lexer: I) -> Result<Declaration, ParseError>
    where
        I: IntoIterator<Item = LexerResult>,
    {
        DefaultParserImpl::new(lexer.into_iter()).parse_declaration()
    }
}

/// function parser wrapper
#[allow(unused)]
pub struct StFunctionParser {}

impl StFunctionParser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse<I: IntoIterator<Item = LexerResult>>(
        &self,
        lexer: I,
    ) -> Result<Statement, ParseError> {
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

    fn next_token(&mut self) -> Result<Rc<LexerItem>, ParseError> {
        match self.next()? {
            Some(x) => Ok(x),
            None => Err(ParseError::UnexpectedEnd),
        }
    }

    /// parse a function
    #[allow(unused)]
    fn parse_function(&mut self) -> Result<Statement, ParseError> {
        let stmts = match self.parse_statement_list()? {
            Some(stmts) => stmts,
            // TODO: error type
            _ => return Err(ParseError::UnexpectedEnd),
        };

        // ensure file end
        match self.next()? {
            None => {}
            _ => return Err(ParseError::InvalidToken(self.next)),
        }

        Ok(stmts)
    }

    /// parse a declaration
    #[allow(unused)]
    fn parse_declaration(&mut self) -> Result<Declaration, ParseError> {
        match self.except_one_of(&[
            TokenType::Type,
            TokenType::Function,
            TokenType::Program,
            TokenType::VarGlobal,
        ])? {
            // pure global variables declare
            TokenType::VarGlobal => {
                let global_vars = self
                    .parse_global_variable_declare_factor()?
                    .unwrap_or(smallvec![]);
                return Ok(Declaration::global_var(Box::new(
                    GlobalVariableDeclare::new(None, global_vars),
                )));
            }

            // type declare
            TokenType::Type => {
                let type_decl = self.parse_type_declaration()?;
                if type_decl.is_none() {
                    todo!()
                }

                let _ = self.except_one_of(&[TokenType::EndType])?;
                Ok(type_decl.unwrap())
            }

            // functions declare
            tok @ TokenType::Function | tok @ TokenType::Program => {
                // name ':'
                let name = self.except_identifier()?;
                let _ = self.except_one_of(&[TokenType::Colon])?;

                // match possible return type
                let ret_type = self.parse_type()?;

                // match possible var decl factor
                let vars = self.parse_variable_declare_factor()?;

                // type class
                let class = if matches!(tok, &TokenType::Function) {
                    let _ = self.except_one_of(&[TokenType::EndFunction])?;
                    DeclareClass::Function
                } else {
                    let _ = self.except_one_of(&[TokenType::EndProgram])?;
                    DeclareClass::Program
                };

                Ok(Declaration::fun(Box::new(FunctionDeclare::new(
                    name,
                    class,
                    ret_type,
                    vars.unwrap_or(smallvec![]),
                ))))
            }
            _ => unreachable!(),
        }
    }

    fn except_identifier(&mut self) -> Result<StString, ParseError> {
        match &*self.next_token()? {
            (_, TokenType::Identifier(ident), _) => Ok(ident.clone()),
            (pos, _, _) => Err(ParseError::InvalidToken(*pos)),
        }
    }

    fn except_one_of<'a>(&mut self, tokens: &'a [TokenType]) -> Result<&'a TokenType, ParseError> {
        let (pos, token, _) = &*self.next_token()?;
        for tok in tokens {
            if tok == token {
                return Ok(tok);
            }
        }

        Err(ParseError::expect_tokens(*pos, tokens))
    }

    fn parse_type(&mut self) -> ParseResult<Rc<Box<dyn Type>>> {
        let pos = self.next;

        match &*self.next_token()? {
            (_, TokenType::Int, _) => Ok(Some(Rc::new(Box::new(IntType::new())))),
            (_, TokenType::Bool, _) => Ok(Some(Rc::new(Box::new(BoolType::new())))),
            (_, TokenType::Identifier(name), _) => {
                Ok(Some(Rc::new(Box::new(UserType::from_name(name.clone())))))
            }
            _ => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// TypeDeclaration: Enum/Alias/Struct
    fn parse_type_declaration(&mut self) -> ParseResult<Declaration> {
        let name = self.except_identifier()?;
        let _ = self.except_one_of(&[TokenType::Colon])?;

        let pos = self.next;
        // alias decl
        if let Some(alias) = self.parse_type()? {
            // ';'
            let _ = self.except_one_of(&[TokenType::Semicolon])?;

            return Ok(Some(Declaration::alias(Box::new(AliasDeclare::new(
                name, alias,
            )))));
        } else {
            self.next = pos;
        }

        // enum decl
        let tok = self.next_token()?;
        if matches!(&*tok, (_, TokenType::LeftParentheses, _)) {
            let mut fields = smallvec![];

            // first field
            if let Some(field) = self.parse_enum_field_decl()? {
                fields.push(field);

                // possible ',' and next field
                loop {
                    let pos = self.next;
                    if !matches!(&*self.next_token()?, (_, TokenType::Comma, _)) {
                        self.next = pos;
                        break;
                    }

                    match self.parse_enum_field_decl()? {
                        Some(field) => fields.push(field),
                        // TODO: error handle
                        _ => return Err(ParseError::UnexpectedEnd),
                    }
                }
            }

            // ')'
            let _ = self.except_one_of(&[TokenType::RightParentheses])?;
            // possible enum type
            let enum_ty = self.parse_type()?;
            // ';'
            let _ = self.except_one_of(&[TokenType::Semicolon])?;

            return Ok(Some(Declaration::enum_(Box::new(EnumDeclare::new(
                name, enum_ty, fields,
            )))));
        }

        // struct decl
        if matches!(&*tok, (_, TokenType::Struct, _)) {
            let fields = self.except_variable_declare_list()?;
            let _ = self.except_one_of(&[TokenType::EndStruct])?;

            return Ok(Some(Declaration::struct_(Box::new(StructDeclare::new(
                name, fields,
            )))));
        }

        Err(ParseError::UnexpectedToken(tok.as_ref().0, vec![]))
    }

    fn parse_enum_field_decl(&mut self) -> ParseResult<Rc<Variable>> {
        let field_name = self.except_identifier()?;
        let pos = self.next;

        if matches!(&*self.next_token()?, (_, TokenType::Assign, _)) {
            match &*self.next_token()? {
                (_, TokenType::Literal(literal), _) => {
                    return Ok(Some(Rc::new(Variable::with_initial(
                        field_name,
                        Some(Box::new(Expression::literal(Box::new(
                            LiteralExpression::new(literal.clone()),
                        )))),
                    ))));
                }
                _ => {}
            }
        }

        self.next = pos;
        return Ok(Some(Rc::new(Variable::with_initial(field_name, None))));
    }

    fn parse_global_variable_declare_factor(&mut self) -> ParseResult<SmallVec8<Rc<Variable>>> {
        let mut v = smallvec![];
        while let Some(mut x) = self.parse_global_variable_group()? {
            v.append(&mut x);
        }

        Ok(Some(v))
    }

    fn parse_global_variable_group(&mut self) -> ParseResult<SmallVec8<Rc<Variable>>> {
        let pos = self.next;
        let tok = self.next()?;

        if !matches!(tok.as_deref(), Some((_, TokenType::VarGlobal, _))) {
            self.next = pos;
            return Ok(None);
        }

        let annotation = self.parse_variable_declare_group_annotation()?;
        let var_list = self.except_variable_declare_list()?;
        let _ = self.except_one_of(&[TokenType::EndVar])?;

        Ok(Some(VariableDeclareGroup::from_variables(
            VariableFlags::GLOBAL | annotation.unwrap_or(VariableFlags::NONE),
            var_list,
        )))
    }

    fn parse_variable_declare_factor(&mut self) -> ParseResult<SmallVec8<Rc<Variable>>> {
        let mut v = smallvec![];
        while let Some(mut x) = self.parse_variable_group()? {
            v.append(&mut x);
        }

        Ok(Some(v))
    }

    fn parse_variable_group(&mut self) -> ParseResult<SmallVec8<Rc<Variable>>> {
        let pos = self.next;
        let group_type = match self.parse_variable_group_start()? {
            Some(x) => x,
            None => {
                self.next = pos;
                return Ok(None);
            }
        };
        let annotation = self.parse_variable_declare_group_annotation()?;
        let var_list = self.except_variable_declare_list()?;
        let _ = self.except_one_of(&[TokenType::EndVar])?;

        Ok(Some(VariableDeclareGroup::from_variables(
            group_type | annotation.unwrap_or(VariableFlags::NONE),
            var_list,
        )))
    }

    fn parse_variable_group_start(&mut self) -> ParseResult<VariableFlags> {
        let x = match &*self.next_token()? {
            (_, TokenType::Var, _) => VariableFlags::NONE,
            // (_, Tok::VarGlobal, _) => VariableFlags::GLOBAL,
            (_, TokenType::VarInput, _) => VariableFlags::INPUT,
            (_, TokenType::VarInOut, _) => VariableFlags::INOUT,
            (_, TokenType::VarOutput, _) => VariableFlags::OUTPUT,
            (_, TokenType::VarTemp, _) => VariableFlags::TEMP,
            (_, TokenType::VarStat, _) => VariableFlags::STATIC,
            _ => return Ok(None),
        };

        Ok(Some(x))
    }

    fn parse_variable_declare_group_annotation(&mut self) -> ParseResult<VariableFlags> {
        let pos = self.next;
        let x = match (&*self.next_token()?).1 {
            TokenType::Retain => VariableFlags::RETAIN,
            TokenType::Persistent => VariableFlags::PERSISTENT,
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        let pos = self.next;
        let y = match (x, &*self.next_token()?) {
            (VariableFlags::RETAIN, (_, TokenType::Persistent, _))
            | (VariableFlags::PERSISTENT, (_, TokenType::Retain, _)) => {
                VariableFlags::RETAINPERSISTENT
            }
            _ => {
                self.next = pos;
                x
            }
        };

        Ok(Some(y))
    }

    fn except_variable_declare_list(&mut self) -> Result<SmallVec8<Rc<Variable>>, ParseError> {
        let mut variables = smallvec![];

        while let Some(mut v) = self.expect_single_line_variable_declare()? {
            variables.append(&mut v);
        }

        Ok(variables)
    }

    fn expect_single_line_variable_declare(&mut self) -> ParseResult<SmallVec8<Rc<Variable>>> {
        let pos = self.next;
        let mut name_list = match &*self.next_token()? {
            (_, TokenType::Identifier(s), _) => smallvec![s.to_owned()],
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        loop {
            let pos = self.next;
            if !matches!(&*self.next_token()?, (_, TokenType::Comma, _)) {
                self.next = pos;
                break;
            }

            name_list.push(self.except_identifier()?);
        }

        let _ = self.except_one_of(&[TokenType::Colon])?;
        let ty = match self.parse_type()? {
            Some(ty) => ty,
            // TODO: expect type
            _ => return Err(ParseError::UnexpectedEnd),
        };
        let _ = self.except_one_of(&[TokenType::Semicolon])?;

        Ok(Some(Variable::multiple_variable_with_type(name_list, ty)))
    }

    fn parse_statement_list(&mut self) -> ParseResult<Statement> {
        let mut statements: Vec<_> = vec![];

        loop {
            match self.parse_statement() {
                Ok(Some(stmt)) => statements.push(stmt),
                Ok(None) => break,
                Err(e) => return Err(e),
            }
        }

        if statements.len() == 1 {
            return Ok(Some(statements.remove(0)));
        }

        Ok(Some(Statement::statement_list(Box::new(statements))))
    }

    /// parse single statement
    fn parse_statement(&mut self) -> ParseResult<Statement> {
        let pos = self.next;
        let tok = self.next()?;

        // IF statement
        if matches!(tok.as_deref(), Some((_, TokenType::If, _))) {
            let if_stmt = self.expect_if_statement()?;
            return Ok(Some(if_stmt));
        }
        self.next = pos;

        if let Some(expr) = self.parse_expr_statement()? {
            return Ok(Some(expr));
        }

        // let pos = self.next;
        // if let Ok(decl) = self.parse_declaration() {
        //     return Ok(Some(Statement::decl(Box::new(decl))));
        // }
        //
        // self.next = pos;
        Ok(None)
    }

    // fn match_val<T, F: FnOnce(Option<T>, T) -> T>(
    //     &mut self,
    //     val: Option<T>,
    //     fallback_pos: usize,
    //     fallback_value: Option<T>,
    //     success_factor: F,
    // ) -> ParseResult<T> {
    //     match val {
    //         Some(v) => Ok(Some(success_factor(fallback_value, v))),
    //         _ => {
    //             self.next = fallback_pos;
    //             Ok(fallback_value)
    //         }
    //     }
    // }

    // 'IF' token already taken
    fn expect_if_statement(&mut self) -> Result<Statement, ParseError> {
        let cond = match self.parse_expression()? {
            Some(expr) => expr,
            // TODO: error type
            _ => return Err(ParseError::UnexpectedEnd),
        };

        let _ = self.except_one_of(&[TokenType::Then])?;

        let then_ctrl = match self.parse_statement_list()? {
            Some(stmts) => stmts,
            // TODO: error type
            _ => return Err(ParseError::UnexpectedEnd),
        };

        let pos = self.next;
        match &*self.next_token()? {
            // IF .. THEN .. END_IF
            (_, TokenType::EndIf, _) => {
                return Ok(Statement::if_stmt(Box::new(IfStatement::from_then(
                    cond, then_ctrl,
                ))));
            }
            // IF .. THEN .. ELSE .. END_IF
            (_, TokenType::Else, _) => {
                let else_ctrl = match self.parse_statement_list()? {
                    Some(stmts) => stmts,
                    // TODO: error type
                    _ => return Err(ParseError::UnexpectedEnd),
                };
                let _ = self.except_one_of(&[TokenType::EndIf])?;

                return Ok(Statement::if_stmt(Box::new(IfStatement::from_then_else(
                    cond, then_ctrl, else_ctrl,
                ))));
            }
            _ => self.next = pos,
        }

        // with ELSEIF list
        let else_if_list = self.parse_elseif_statement_list()?;

        todo!()
    }

    fn parse_elseif_statement_list(&mut self) -> ParseResult<Vec<ElseIfStatement>> {
        todo!()
    }

    fn parse_expr_statement(&mut self) -> ParseResult<Statement> {
        let pos = self.next;
        let expr = match self.parse_expression()? {
            Some(expr) => expr,
            None => {
                self.next = pos;
                return Ok(None);
            }
        };

        if !matches!(self.next()?.as_deref(), Some((_, TokenType::Semicolon, _))) {
            self.next = pos;
            return Ok(None);
        }

        Ok(Some(Statement::expr(Box::new(ExprStatement::new(expr)))))
    }

    fn parse_op_expression(&mut self) -> ParseResult<Box<Expression>> {
        todo!()
    }

    /// Expr: Expr ":=" BitOrExpr
    ///     | BitOrExpr
    ///
    /// Expr: BitOrExpr expr'
    fn parse_expression(&mut self) -> ParseResult<Expression> {
        let pos = self.next;
        if let Some(bitor) = self.parse_bitor_expression()? {
            return match self.parse_expression_fix()? {
                Some(fix) => Ok(Some(Expression::assign(Box::new(AssignExpression::new(
                    bitor, fix,
                ))))),
                None => Ok(Some(bitor)),
            };
        }

        self.next = pos;
        Ok(None)
    }

    /// expr': ":=" BitOrExpr expr' | ε
    fn parse_expression_fix(&mut self) -> ParseResult<Expression> {
        let pos = self.next;
        if !matches!(self.next()?.as_deref(), Some((_, TokenType::Assign, _))) {
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
    fn parse_bitor_expression(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        if let Some(bitor) = self.parse_xor_expression()? {
            if let Some(fix) = self.parse_bitor_expression_fix()? {
                Ok(Some(Expression::operator(Box::new(
                    OperatorExpression::new(TokenType::BitOr, vec![bitor, fix]),
                ))))
            } else {
                Ok(Some(bitor))
            }
        } else {
            self.next = pos;
            Ok(None)
        }
    }

    fn parse_bitor_expression_fix(&mut self) -> ParseResult<Expression> {
        let pos = self.next;
        if !matches!(self.next()?.as_deref(), Some((_, TokenType::BitOr, _))) {
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
    fn parse_xor_expression(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        match self.parse_bitand_expression()? {
            Some(bitand) => match self.parse_xor_expression_fix()? {
                Some(fix) => Ok(Some(Expression::operator(Box::new(
                    OperatorExpression::new(TokenType::Xor, vec![bitand, fix]),
                )))),
                None => Ok(Some(bitand)),
            },
            None => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    fn parse_xor_expression_fix(&mut self) -> ParseResult<Expression> {
        let pos = self.next;
        if !matches!(self.next()?.as_deref(), Some((_, TokenType::Xor, _))) {
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
    fn parse_bitand_expression(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        match self.parse_equ_expr()? {
            Some(equ) => match self.parse_bitand_expression_fix()? {
                Some(fix) => Ok(Some(Expression::operator(Box::new(
                    OperatorExpression::new(TokenType::BitAnd, vec![equ, fix]),
                )))),
                None => Ok(Some(equ)),
            },
            None => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    fn parse_bitand_expression_fix(&mut self) -> ParseResult<Expression> {
        let pos = self.next;
        if !matches!(self.next()?.as_deref(), Some((_, TokenType::BitAnd, _))) {
            self.next = pos;
            return Ok(None);
        }

        self.parse_bitand_expression()
    }

    /// EquExpr: EquExpr EquOp CmpExpr
    ///     | CmpExpr
    ///
    /// EquExpr: CmpExpr equexpr'
    fn parse_equ_expr(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        match self.parse_cmp_expr()? {
            Some(equ) => match self.parse_equ_expr_fix()? {
                Some((tok, fix)) => Ok(Some(Expression::operator(Box::new(
                    OperatorExpression::new(tok, vec![equ, fix]),
                )))),
                None => Ok(Some(equ)),
            },
            None => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// equexpr': EquOp CmpExpr equexpr' | ε
    fn parse_equ_expr_fix(&mut self) -> ParseResult<(TokenType, Expression)> {
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
                let tail = Expression::operator(Box::new(OperatorExpression::new(
                    tok,
                    vec![cmp_expr, fix],
                )));
                Ok(Some((current_tok, tail)))
            }
            _ => {
                self.next = pos;
                Ok(Some((current_tok, cmp_expr)))
            }
        }
    }

    fn parse_equ_op(&mut self) -> ParseResult<TokenType> {
        let pos = self.next;

        match self.next()?.as_deref() {
            Some((_, tok @ TokenType::Equal, _)) | Some((_, tok @ TokenType::NotEqual, _)) => {
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
    fn parse_cmp_expr(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        match self.parse_op_expr()? {
            Some(cmp) => match self.parse_cmp_expr_fix()? {
                Some((tok, fix)) => Ok(Some(Expression::operator(Box::new(
                    OperatorExpression::new(tok, vec![cmp, fix]),
                )))),
                None => Ok(Some(cmp)),
            },
            None => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// cmpexpr': CmpOp OpExpr cmpexpr' | ε
    fn parse_cmp_expr_fix(&mut self) -> ParseResult<(TokenType, Expression)> {
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
                let tail = Expression::operator(Box::new(OperatorExpression::new(
                    tok,
                    vec![op_expr, fix],
                )));
                Ok(Some((current_tok, tail)))
            }
            _ => {
                self.next = pos;
                Ok(Some((current_tok, op_expr)))
            }
        }
    }

    fn parse_cmp_op(&mut self) -> ParseResult<TokenType> {
        let pos = self.next;

        match self.next()?.as_deref() {
            Some((_, tok @ TokenType::Greater, _))
            | Some((_, tok @ TokenType::GreaterEqual, _))
            | Some((_, tok @ TokenType::Less, _))
            | Some((_, tok @ TokenType::LessEqual, _)) => Ok(Some(tok.clone())),
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
    fn parse_op_expr(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        if let Some(factor) = self.parse_factor()? {
            if let Some((tok, fix)) = self.parse_op_expr_fix()? {
                Ok(Some(Expression::operator(Box::new(
                    OperatorExpression::new(tok, vec![factor, fix]),
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
    fn parse_op_expr_fix(&mut self) -> ParseResult<(TokenType, Expression)> {
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
            let tail =
                Expression::operator(Box::new(OperatorExpression::new(tok, vec![factor, fix])));
            Ok(Some((current_tok, tail)))
        } else {
            self.next = pos;
            Ok(Some((current_tok, factor)))
        }
    }

    // + / -
    fn parse_expr_op(&mut self) -> ParseResult<TokenType> {
        let pos = self.next;

        match self.next()?.as_deref() {
            Some((_, tok @ TokenType::Plus, _)) | Some((_, tok @ TokenType::Minus, _)) => {
                Ok(Some(tok.clone()))
            }
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
    fn parse_factor(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        if let Some(power) = self.parse_power_expr()? {
            if let Some((tok, fix)) = self.parse_factor_fix()? {
                Ok(Some(Expression::operator(Box::new(
                    OperatorExpression::new(tok, vec![power, fix]),
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
    fn parse_factor_fix(&mut self) -> ParseResult<(TokenType, Expression)> {
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
            let tail =
                Expression::operator(Box::new(OperatorExpression::new(tok, vec![power, fix])));
            Ok(Some((current_tok, tail)))
        } else {
            self.next = pos;
            Ok(Some((current_tok, power)))
        }
    }

    // '*' '/' 'MOD'
    fn parse_factor_op(&mut self) -> ParseResult<TokenType> {
        let pos = self.next;

        match self.next()?.as_deref() {
            Some((_, tok @ TokenType::Multiply, _))
            | Some((_, tok @ TokenType::Division, _))
            | Some((_, tok @ TokenType::Mod, _)) => Ok(Some(tok.clone())),
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
    fn parse_power_expr(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        match self.parse_unary_factor()? {
            Some(unary) => match self.parse_power_expr_fix()? {
                Some(fix) => Ok(Some(Expression::operator(Box::new(
                    OperatorExpression::new(TokenType::Power, vec![unary, fix]),
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
    fn parse_power_expr_fix(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        if !matches!(self.next()?.as_deref(), Some((_, TokenType::Power, _))) {
            self.next = pos;
            return Ok(None);
        }

        let unary = match self.parse_unary_factor()? {
            Some(unary) => unary,
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        let pos = self.next;
        if let Some(fix) = self.parse_power_expr_fix()? {
            let tail = Expression::operator(Box::new(OperatorExpression::new(
                TokenType::Power,
                vec![unary, fix],
            )));
            Ok(Some(tail))
        } else {
            self.next = pos;
            Ok(Some(unary))
        }
    }

    /// UnaryFactor: UnaryOp CompoFactor
    ///     | CompoFactor
    fn parse_unary_factor(&mut self) -> ParseResult<Expression> {
        let pos = self.next;
        let unary_op = self.parse_unary_op()?;
        let compo_factor = self.parse_compo_factor()?;

        match (unary_op, compo_factor) {
            (Some(op), Some(factor)) => Ok(Some(Expression::operator(Box::new(
                OperatorExpression::new(op, vec![factor]),
            )))),
            (None, Some(factor)) => Ok(Some(factor)),
            _ => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    // '-' 'NOT'
    fn parse_unary_op(&mut self) -> ParseResult<TokenType> {
        let pos = self.next;

        match self.next()?.as_deref() {
            Some((_, tok @ TokenType::Not, _)) | Some((_, tok @ TokenType::Minus, _)) => {
                Ok(Some(tok.clone()))
            }
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
    fn parse_compo_factor(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        match self.parse_term_expr()? {
            Some(term) => match self.parse_compo_factor_fix()? {
                Some(fix) => Ok(Some(Expression::compo(Box::new(
                    CompoAccessExpression::new(term, fix),
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
    fn parse_compo_factor_fix(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        if !matches!(self.next()?.as_deref(), Some((_, TokenType::Access, _))) {
            self.next = pos;
            return Ok(None);
        }

        self.parse_compo_factor()
    }

    fn parse_term_expr(&mut self) -> ParseResult<Expression> {
        if let Some(val) = self.parse_variable_expr()? {
            return Ok(Some(val));
        }

        if let Some(literal) = self.parse_literal_expr()? {
            return Ok(Some(literal));
        }

        let pos = self.next;
        if matches!(
            self.next()?.as_deref(),
            Some((_, TokenType::LeftParentheses, _))
        ) {
            if let Some(expr) = self.parse_expression()? {
                let _ = self.except_one_of(&[TokenType::RightParentheses])?;
                return Ok(Some(expr));
            }
        }

        self.next = pos;
        Ok(None)
    }

    fn parse_literal_expr(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        match self.next()?.as_deref() {
            Some((_, TokenType::Literal(val), _)) => {
                return Ok(Some(Expression::literal(Box::new(LiteralExpression::new(
                    val.clone(),
                )))))
            }
            _ => {}
        }

        self.next = pos;
        Ok(None)
    }

    fn parse_variable_expr(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        let ident = match self.next()?.as_deref() {
            Some((_, TokenType::Identifier(ident), _)) => ident.clone(),
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        Ok(Some(Expression::variable(Box::new(
            VariableExpression::new(ident),
        ))))
    }
}
