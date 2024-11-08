use crate::ast::*;
use crate::parser::token::Token;
use crate::parser::*;

use smallvec::smallvec;
use std::sync::Arc;

///
/// A  ->  Aα | β
///
/// A  ->  βA'
/// A' ->  αA' | ε
///

/// Parser functions result
type ParseResult<T> = Result<Option<T>, ParseError>;

/// declaration parser wrapper
#[derive(Default)]
pub struct DefaultParser {}

impl DefaultParser {
    pub fn new() -> Self {
        Default::default()
    }
}

impl ParserTrait for DefaultParser {
    #[inline]
    fn parse_decl(&self, lexer: &mut StLexer) -> Result<Declaration, ParseError> {
        DefaultParserImpl::new(lexer.into_iter()).parse_declaration()
    }

    #[inline]
    fn parse_stmt(&self, lexer: &mut StLexer) -> Result<Statement, ParseError> {
        DefaultParserImpl::new(lexer.into_iter()).parse_function()
    }

    #[inline]
    fn parse_literal(&self, lexer: &mut StLexer) -> Result<LiteralExpression, ParseError> {
        let parse_result = DefaultParserImpl::new(lexer.into_iter()).parse_literal_expr()?;
        if let Some(expr) = parse_result {
            return Ok(expr);
        }

        // TODO: Parse no result
        Err(ParseError::InvalidToken(0))
    }

    #[inline]
    fn parse_expression(&self, lexer: &mut StLexer) -> Result<Expression, ParseError> {
        match DefaultParserImpl::new(lexer.into_iter()).parse_expression()? {
            Some(expr) => Ok(expr),
            None => Err(ParseError::InvalidToken(0)),
        }
    }
}

/// LL(*) parser implementation
struct DefaultParserImpl<I: Iterator<Item = LexerResult>> {
    lexer: I,
    /// next token index
    next: usize,
    /// save all tokens, because we need to backtracking
    tokens: Vec<Token>,
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
    fn next(&mut self) -> Result<Option<&Token>, ParseError> {
        while self.next >= self.tokens.len() {
            match self.lexer.next() {
                Some(Ok(item)) => self.tokens.push(item),
                Some(Err(e)) => return Err(ParseError::LexerError(e)),
                None => return Ok(None),
            }
        }

        let r = Ok(Some(&self.tokens[self.next]));
        self.next += 1;

        r
    }

    /// test has next token
    fn has_next(&mut self) -> bool {
        // fill buff
        while self.next >= self.tokens.len() {
            match self.lexer.next() {
                Some(Ok(item)) => self.tokens.push(item),
                _ => return false,
            }
        }

        true
    }

    #[inline]
    fn next_token(&mut self) -> Result<&Token, ParseError> {
        match self.next()? {
            Some(x) => Ok(x),
            None => Err(ParseError::UnexpectedEnd),
        }
    }

    #[inline]
    fn next_kind(&mut self) -> Result<&TokenKind, ParseError> {
        self.next_token().map(|x| &x.kind)
    }

    /// parse a function
    fn parse_function(&mut self) -> Result<Statement, ParseError> {
        let stmts = match self.parse_statement_list()? {
            Some(stmts) => stmts,
            // TODO: error type
            _ => return Err(ParseError::UnexpectedEnd),
        };

        // ensure file end
        match self.next()? {
            None => {}
            Some(tok) => return Err(ParseError::InvalidTokenAt(format!("{:?}", tok))),
        }

        Ok(stmts)
    }

    /// parse a declaration
    fn parse_declaration(&mut self) -> Result<Declaration, ParseError> {
        let pos = self.next;
        let except_token = self.except_one_of(&[
            TokenKind::Type,
            TokenKind::Function,
            TokenKind::Program,
            TokenKind::VarGlobal,
        ])?;

        match except_token.kind.clone() {
            // pure global variables declare
            TokenKind::VarGlobal => {
                self.next = pos;
                let global_vars = self
                    .parse_global_variable_declare_factor()?
                    .unwrap_or(smallvec![]);
                Ok(Declaration::global_var(Box::new(
                    GlobalVariableDeclare::new(None, global_vars),
                )))
            }

            // type declare
            TokenKind::Type => {
                let type_decl = self.parse_type_declaration()?;
                if type_decl.is_none() {
                    todo!()
                }

                let _ = self.except_one_of(&[TokenKind::EndType])?;
                Ok(type_decl.unwrap())
            }

            // functions declare
            tok @ TokenKind::Function | tok @ TokenKind::Program => {
                // name ':'
                let name = self.except_identifier()?;
                let _ = self.except_one_of(&[TokenKind::Colon])?;

                // match possible return type
                let ret_type = self.parse_type()?;

                // match possible var decl factor
                let vars = self.parse_variable_declare_factor()?;

                // type class
                let class = if matches!(tok, TokenKind::Function) {
                    let _ = self.except_one_of(&[TokenKind::EndFunction])?;
                    DeclareClass::Function
                } else {
                    let _ = self.except_one_of(&[TokenKind::EndProgram])?;
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
        let tok = self.next_token()?;
        match &tok.kind {
            TokenKind::Identifier(ident) => Ok(ident.clone()),
            _ => Err(ParseError::InvalidToken(0)),
        }
    }

    fn except_one(&mut self, token: TokenKind) -> Result<&Token, ParseError> {
        self.except_one_of(&[token])
    }

    fn except_one_of(&mut self, tokens: &[TokenKind]) -> Result<&Token, ParseError> {
        let tok = self.next_token()?;
        for want in tokens {
            if tok.kind.kind_match(want) {
                return Ok(tok);
            }
        }

        Err(ParseError::expect_tokens(0, tokens))
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        let pos = self.next;
        let token = self.next_token()?;

        match &token.kind {
            TokenKind::Array => {
                self.next = pos;
                self.parse_array_type()
            }
            TokenKind::Bit => Ok(Some(BitType::new_type())),
            TokenKind::Bool => Ok(Some(BoolType::new_type())),
            TokenKind::Byte => Ok(Some(ByteType::new_type())),
            TokenKind::Int => Ok(Some(IntType::new_type())),
            TokenKind::Real => Ok(Some(RealType::new_type())),
            TokenKind::Identifier(ident) => Ok(Some(UserType::from_name(ident.clone()).into())),
            _ => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// Parse range expr like: a..b
    fn parse_range_expression(&mut self) -> ParseResult<RangeExpression> {
        let lower = match self.parse_expression()? {
            Some(expr) => expr,
            _ => return Ok(None),
        };

        let _ = self.except_one(TokenKind::DotRange)?;

        let upper = match self.parse_expression()? {
            Some(expr) => expr,
            _ => return Ok(None),
        };

        Ok(Some(RangeExpression::new(lower, upper)))
    }

    /// parse: a..b, c..d
    fn parse_array_dimensions(&mut self) -> ParseResult<SmallVec3<RangeExpression>> {
        let mut group = smallvec![];

        loop {
            match self.parse_range_expression()? {
                Some(range) => group.push(range),
                _ => {
                    break;
                }
            }

            let pos = self.next;
            if self.except_one(TokenKind::Comma).is_err() {
                self.next = pos;
                break;
            }
        }

        if group.is_empty() {
            Ok(None)
        } else {
            Ok(Some(group))
        }
    }

    /// Parse ArrayType
    fn parse_array_type(&mut self) -> ParseResult<Type> {
        let _ = self.except_one(TokenKind::Array)?;
        let _ = self.except_one(TokenKind::LeftBracket)?;

        let dims = self.parse_array_dimensions()?;

        let _ = self.except_one(TokenKind::RightBracket)?;
        let _ = self.except_one(TokenKind::Of)?;

        if let Some(base_type) = self.parse_type()? {
            Ok(Some(ArrayType::new(base_type, dims.unwrap()).into()))
        } else {
            // TODO: err
            Ok(None)
        }
    }

    /// TypeDeclaration: Enum/Alias/Struct
    fn parse_type_declaration(&mut self) -> ParseResult<Declaration> {
        let name = self.except_identifier()?;
        let _ = self.except_one_of(&[TokenKind::Colon])?;

        let pos = self.next;
        // alias decl
        if let Some(alias) = self.parse_type()? {
            // ';'
            let _ = self.except_one_of(&[TokenKind::Semicolon])?;

            return Ok(Some(Declaration::new_alias(name, alias)));
        } else {
            self.next = pos;
        }

        // enum decl
        let tok = self.next_kind()?;
        if matches!(tok, TokenKind::LeftParentheses) {
            let mut fields = smallvec![];

            // first field
            if let Some(field) = self.parse_enum_field_decl()? {
                fields.push(field);

                // possible ',' and next field
                loop {
                    let pos = self.next;
                    if !matches!(*self.next_kind()?, TokenKind::Comma) {
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
            let _ = self.except_one_of(&[TokenKind::RightParentheses])?;
            // possible enum type
            let enum_ty = self.parse_type()?;
            // ';'
            let _ = self.except_one_of(&[TokenKind::Semicolon])?;

            return Ok(Some(Declaration::new_enum(name, enum_ty, fields)));
        }

        // struct decl
        if matches!(tok, TokenKind::Struct) {
            let fields = self.except_variable_declare_list()?;
            let _ = self.except_one_of(&[TokenKind::EndStruct])?;

            return Ok(Some(Declaration::new_struct(name, fields)));
        }

        Err(ParseError::UnexpectedToken(0, vec![format!("{:?}", tok)]))
    }

    fn parse_enum_field_decl(&mut self) -> ParseResult<Arc<Variable>> {
        let field_name = self.except_identifier()?;
        let pos = self.next;

        if matches!(*self.next_kind()?, TokenKind::Assign) {
            if let TokenKind::Literal(literal) = self.next_kind()? {
                return Ok(Some(Arc::new(Variable::with_initial(
                    field_name,
                    Box::new(Expression::new_literal(literal.clone())),
                ))));
            }
        }

        self.next = pos;
        Ok(Some(Arc::new(Variable::new(field_name))))
    }

    fn parse_global_variable_declare_factor(&mut self) -> ParseResult<SmallVec8<Arc<Variable>>> {
        let mut v = smallvec![];
        loop {
            if !self.has_next() {
                break;
            }

            if let Some(mut x) = self.parse_global_variable_group()? {
                v.append(&mut x);
            } else {
                panic!()
            }
        }

        // TODO: should be fail if 'v' is empty
        Ok(Some(v))
    }

    fn parse_global_variable_group(&mut self) -> ParseResult<SmallVec8<Arc<Variable>>> {
        let pos = self.next;
        let tok = self.next_kind()?;

        if !matches!(*tok, TokenKind::VarGlobal) {
            self.next = pos;
            return Ok(None);
        }

        let annotation = self.parse_variable_declare_group_annotation()?;
        let var_list = self.except_variable_declare_list()?;
        let _ = self.except_one_of(&[TokenKind::EndVar])?;

        Ok(Some(VariableDeclareGroup::from_variables(
            VariableFlags::GLOBAL | annotation.unwrap_or(VariableFlags::NONE),
            var_list,
        )))
    }

    fn parse_variable_declare_factor(&mut self) -> ParseResult<SmallVec8<Arc<Variable>>> {
        let mut v = SmallVec8::new();
        while let Some(mut x) = self.parse_variable_group()? {
            v.append(&mut x);
        }

        Ok(Some(v))
    }

    fn parse_variable_group(&mut self) -> ParseResult<SmallVec8<Arc<Variable>>> {
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
        let _ = self.except_one_of(&[TokenKind::EndVar])?;

        Ok(Some(VariableDeclareGroup::from_variables(
            group_type | annotation.unwrap_or(VariableFlags::NONE),
            var_list,
        )))
    }

    fn parse_variable_group_start(&mut self) -> ParseResult<VariableFlags> {
        let x = match *self.next_kind()? {
            TokenKind::Var => VariableFlags::NONE,
            TokenKind::VarGlobal => VariableFlags::GLOBAL,
            TokenKind::VarInput => VariableFlags::INPUT,
            TokenKind::VarInOut => VariableFlags::INOUT,
            TokenKind::VarOutput => VariableFlags::OUTPUT,
            TokenKind::VarTemp => VariableFlags::TEMP,
            TokenKind::VarStat => VariableFlags::STATIC,
            _ => return Ok(None),
        };

        Ok(Some(x))
    }

    fn parse_variable_declare_group_annotation(&mut self) -> ParseResult<VariableFlags> {
        let pos = self.next;
        let x = match *self.next_kind()? {
            TokenKind::Retain => VariableFlags::RETAIN,
            TokenKind::Persistent => VariableFlags::PERSISTENT,
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        let pos = self.next;
        let y = match (x, self.next_kind()?) {
            (VariableFlags::RETAIN, TokenKind::Persistent)
            | (VariableFlags::PERSISTENT, TokenKind::Retain) => VariableFlags::RETAINPERSISTENT,
            _ => {
                self.next = pos;
                x
            }
        };

        Ok(Some(y))
    }

    fn except_variable_declare_list(&mut self) -> Result<SmallVec8<Arc<Variable>>, ParseError> {
        let mut variables = smallvec![];

        while let Some(mut v) = self.expect_single_line_variable_declare()? {
            variables.append(&mut v);
        }

        Ok(variables)
    }

    fn expect_single_line_variable_declare(&mut self) -> ParseResult<SmallVec8<Arc<Variable>>> {
        let pos = self.next;
        let mut name_list = match self.next_kind()? {
            TokenKind::Identifier(s) => smallvec![s.to_owned()],
            _ => {
                self.next = pos;
                return Ok(None);
            }
        };

        loop {
            let pos = self.next;
            if !matches!(self.next_kind()?, TokenKind::Comma) {
                self.next = pos;
                break;
            }

            name_list.push(self.except_identifier()?);
        }

        let _ = self.except_one_of(&[TokenKind::Colon])?;
        let ty = match self.parse_type()? {
            Some(ty) => ty,
            // TODO: expect type
            _ => return Err(ParseError::UnexpectedEnd),
        };
        let _ = self.except_one_of(&[TokenKind::Semicolon])?;

        Ok(Some(Variable::multiple_variable_with_type(name_list, ty)))
    }

    fn parse_statement_list(&mut self) -> ParseResult<Statement> {
        let mut statements: Vec<_> = vec![];

        loop {
            let p = self.next;
            match self.next_token() {
                Err(ParseError::UnexpectedEnd) => {
                    break;
                }
                _ => {
                    self.next = p;
                }
            }

            match self.parse_statement()? {
                Some(stmt) => {
                    statements.push(stmt);
                }
                None => break,
            }
        }

        // extract statement list to single statement
        if statements.len() == 1 {
            return Ok(Some(statements.remove(0)));
        }

        Ok(Some(Statement::statement_list(Box::new(statements))))
    }

    /// parse single statement
    fn parse_statement(&mut self) -> ParseResult<Statement> {
        let pos = self.next;
        let tok = self.next_kind()?;

        // IF statement
        if matches!(tok, TokenKind::If) {
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

        let _ = self.except_one_of(&[TokenKind::Then])?;

        let then_ctrl = match self.parse_statement_list()? {
            Some(stmts) => stmts,
            // TODO: error type
            _ => return Err(ParseError::UnexpectedEnd),
        };

        let pos = self.next;
        match self.next_kind()? {
            // IF .. THEN .. END_IF
            TokenKind::EndIf => {
                return Ok(Statement::if_stmt(Box::new(IfStatement::from_then(
                    cond, then_ctrl,
                ))));
            }
            // IF .. THEN .. ELSE .. END_IF
            TokenKind::Else => {
                let else_ctrl = match self.parse_statement_list()? {
                    Some(stmts) => stmts,
                    // TODO: error type
                    _ => return Err(ParseError::UnexpectedEnd),
                };
                let _ = self.except_one(TokenKind::EndIf)?;

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

        if !matches!(self.next_kind()?, TokenKind::Semicolon) {
            self.next = pos;
            return Ok(None);
        }

        Ok(Some(Statement::new_expr(expr)))
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
                Some(fix) => Ok(Some(Expression::new_assign(bitor, fix))),
                None => Ok(Some(bitor)),
            };
        }

        self.next = pos;
        Ok(None)
    }

    /// expr': ":=" BitOrExpr expr' | ε
    fn parse_expression_fix(&mut self) -> ParseResult<Expression> {
        let pos = self.next;
        if !matches!(self.next_kind()?, TokenKind::Assign) {
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
                Ok(Some(Expression::new_operator2(Operator::BitOr, bitor, fix)))
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
        if !matches!(self.next_kind()?, TokenKind::BitOr) {
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
                Some(fix) => Ok(Some(Expression::new_operator2(Operator::Xor, bitand, fix))),
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
        if !matches!(self.next_kind()?, TokenKind::Xor) {
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
                Some(fix) => Ok(Some(Expression::new_operator2(Operator::BitAnd, equ, fix))),
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
        if !matches!(self.next_kind()?, TokenKind::BitAnd) {
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
                Some((tk, fix)) => Ok(Some(Expression::new_operator2(tk.into(), equ, fix))),
                None => Ok(Some(equ)),
            },
            None => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// equexpr': EquOp CmpExpr equexpr' | ε
    fn parse_equ_expr_fix(&mut self) -> ParseResult<(TokenKind, Expression)> {
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
                let tail = Expression::new_operator2(tok.into(), cmp_expr, fix);
                Ok(Some((current_tok, tail)))
            }
            _ => {
                self.next = pos;
                Ok(Some((current_tok, cmp_expr)))
            }
        }
    }

    fn parse_equ_op(&mut self) -> ParseResult<TokenKind> {
        let pos = self.next;

        match self.next_kind()? {
            tok @ TokenKind::Equal | tok @ TokenKind::NotEqual => Ok(Some(tok.clone())),
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
                Some((tok, fix)) => Ok(Some(Expression::new_operator2(tok.into(), cmp, fix))),
                None => Ok(Some(cmp)),
            },
            None => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    /// cmpexpr': CmpOp OpExpr cmpexpr' | ε
    fn parse_cmp_expr_fix(&mut self) -> ParseResult<(TokenKind, Expression)> {
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
                let tail = Expression::new_operator2(tok.into(), op_expr, fix);
                Ok(Some((current_tok, tail)))
            }
            _ => {
                self.next = pos;
                Ok(Some((current_tok, op_expr)))
            }
        }
    }

    fn parse_cmp_op(&mut self) -> ParseResult<TokenKind> {
        let pos = self.next;

        match self.next_kind()? {
            tok @ TokenKind::Greater
            | tok @ TokenKind::GreaterEqual
            | tok @ TokenKind::Less
            | tok @ TokenKind::LessEqual => Ok(Some(tok.clone())),
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
                Ok(Some(Expression::new_operator2(tok.into(), factor, fix)))
            } else {
                Ok(Some(factor))
            }
        } else {
            self.next = pos;
            Ok(None)
        }
    }

    /// opexpr': ExprOp Factor opexpr' | ε
    fn parse_op_expr_fix(&mut self) -> ParseResult<(TokenKind, Expression)> {
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
            let tail = Expression::new_operator2(tok.into(), factor, fix);
            Ok(Some((current_tok, tail)))
        } else {
            self.next = pos;
            Ok(Some((current_tok, factor)))
        }
    }

    // + / -
    fn parse_expr_op(&mut self) -> ParseResult<TokenKind> {
        let pos = self.next;

        match self.next_kind()? {
            tok @ TokenKind::Plus | tok @ TokenKind::Minus => Ok(Some(tok.clone())),
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
                Ok(Some(Expression::new_operator2(tok.into(), power, fix)))
            } else {
                Ok(Some(power))
            }
        } else {
            self.next = pos;
            Ok(None)
        }
    }

    /// factor': FactorOp PowerExpr factor' | ε
    fn parse_factor_fix(&mut self) -> ParseResult<(TokenKind, Expression)> {
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
            let tail = Expression::new_operator2(tok.into(), power, fix);
            Ok(Some((current_tok, tail)))
        } else {
            self.next = pos;
            Ok(Some((current_tok, power)))
        }
    }

    // '*' '/' 'MOD'
    fn parse_factor_op(&mut self) -> ParseResult<TokenKind> {
        let pos = self.next;

        match self.next_kind()? {
            tok @ TokenKind::Multiply | tok @ TokenKind::Division | tok @ TokenKind::Mod => {
                Ok(Some(tok.clone()))
            }
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
                Some(fix) => Ok(Some(Expression::new_operator2(Operator::Power, unary, fix))),
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

        if !matches!(self.next_kind()?, TokenKind::Power) {
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
            let tail = Expression::new_operator2(Operator::Power, unary, fix);
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
            (Some(op), Some(factor)) => {
                Ok(Some(Expression::new_operator(op.into(), smallvec![factor])))
            }
            (None, Some(factor)) => Ok(Some(factor)),
            _ => {
                self.next = pos;
                Ok(None)
            }
        }
    }

    // '-' 'NOT'
    fn parse_unary_op(&mut self) -> ParseResult<TokenKind> {
        let pos = self.next;

        match self.next_kind()? {
            tok @ TokenKind::Not | tok @ TokenKind::Minus => Ok(Some(tok.clone())),
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
                Some(fix) => Ok(Some(Expression::new_compo(term, fix))),
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

        if !matches!(self.next_kind()?, TokenKind::DotAccess) {
            self.next = pos;
            return Ok(None);
        }

        self.parse_compo_factor()
    }

    fn parse_term_expr(&mut self) -> ParseResult<Expression> {
        if let Some(val) = self.parse_variable_expr()? {
            let p = self.next;
            let tk = self.next_kind()?;

            // val ^ (...
            if matches!(tk, TokenKind::LeftParentheses) {
                return self.parse_call_expr_args(val);
            }

            self.next = p;
            return Ok(Some(val));
        }

        if let Some(literal) = self.parse_literal_expression()? {
            return Ok(Some(literal));
        }

        let pos = self.next;
        if matches!(self.next_kind()?, TokenKind::LeftParentheses) {
            if let Some(expr) = self.parse_expression()? {
                let _ = self.except_one_of(&[TokenKind::RightParentheses])?;
                return Ok(Some(expr));
            }
        }

        self.next = pos;
        Ok(None)
    }

    // function name is already taken: fun( ^ ...)
    fn parse_call_expr_args(&mut self, callee: Expression) -> ParseResult<Expression> {
        let origin_pos = self.next;

        // last token is comma
        // let mut comma = false;
        let mut args: SmallVec8<Expression> = smallvec![];
        loop {
            let p = self.next;
            let nk = self.next_kind()?;
            if matches!(nk, TokenKind::RightParentheses) {
                return Ok(Some(Expression::call(Box::new(
                    CallExpression::with_arguments(callee, args),
                ))));
            }

            self.next = p;
            // argument is needed
            if let Ok(Some(arg_assign)) = self.parse_argument_expression() {
                // comma = false;
                args.push(arg_assign);

                // after argument been eaten, a Comma is necessary
                let nk = self.next_kind()?;
                match nk {
                    // continue to eat new argument expr
                    // TokenKind::Comma if !comma => {
                    //     comma = true;
                    //     continue;
                    // }
                    TokenKind::Comma => {
                        // TODO: dup comma
                        continue;
                    }
                    // call finished
                    TokenKind::RightParentheses => {
                        return Ok(Some(Expression::call(Box::new(
                            CallExpression::with_arguments(callee, args),
                        ))));
                    }
                    _ => break,
                }
            } else {
                break;
            }
        }

        self.next = origin_pos;
        Ok(None)
    }

    fn parse_argument_expression(&mut self) -> ParseResult<Expression> {
        Ok(self
            .parse_literal_expression()?
            .or(self.parse_argument_assign_expr()?))
    }

    // a => b
    // a := b
    fn parse_argument_assign_expr(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        let lhs = self.parse_variable_expr()?;
        if lhs.is_none() {
            self.next = pos;
            return Ok(None);
        }

        let pos = self.next;
        let assign_type = match self.next_kind()? {
            TokenKind::Assign => AssignType::Assign,
            TokenKind::AssignRight => AssignType::AssignRight,
            _ => {
                self.next = pos;
                return Ok(lhs);
            }
        };

        let rhs = self.parse_expression()?;
        Ok(Some(Expression::assign(Box::new(
            AssignExpression::with_type(lhs.unwrap(), rhs.unwrap(), assign_type),
        ))))
    }

    #[inline]
    fn parse_literal_expression(&mut self) -> ParseResult<Expression> {
        let result = self.parse_literal_expr()?;
        match result {
            Some(literal_expr) => Ok(Some(Expression::literal(Box::new(literal_expr)))),
            None => Ok(None),
        }
    }

    fn parse_literal_expr(&mut self) -> ParseResult<LiteralExpression> {
        let pos = self.next;

        if let TokenKind::Literal(val) = self.next_kind()? {
            return Ok(Some(LiteralExpression::new(val.clone())));
        }

        self.next = pos;
        Ok(None)
    }

    fn parse_variable_expr(&mut self) -> ParseResult<Expression> {
        let pos = self.next;

        match self.next_kind()? {
            TokenKind::Identifier(ident) => Ok(Some(Expression::new_variable(ident.clone()))),
            _ => {
                self.next = pos;
                Ok(None)
            }
        }
    }
}
