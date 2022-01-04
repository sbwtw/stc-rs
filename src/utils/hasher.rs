use crate::ast::*;
use crate::parser::{LiteralValue, Tok};
use crc::{Crc, Digest, CRC_32_ISO_HDLC};
use std::cell::RefCell;
use std::hash::{Hash, Hasher};

#[allow(unused)]
const CRC32: Crc<u32> = Crc::<u32>::new(&CRC_32_ISO_HDLC);

/// ensure different statement has different hash code
#[derive(Hash)]
#[allow(dead_code)]
enum VisitType {
    Literal,
    Variable,
    StatementList,
    ExprStatement,
    IfStatement,
    ThenStatement,
    ElseIfStatement,
    ElseStatement,
    DeclarationStatement,
    OperatorExpression,
    Operand,
    AssignExpression,
    CompoAccessExpression,
}

trait MyHash {
    fn hash<H: Hasher>(&self, state: &mut H);
}

impl MyHash for LiteralValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        format!("{}", self).hash(state)
    }
}

impl MyHash for Variable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name().hash(state);
        self.flags().hash(state);
        if let Some(ty) = self.ty() {
            MyHash::hash(ty.as_ref(), state)
        }
    }
}

impl MyHash for &dyn Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let type_class = format!("{}", self.type_class());
        type_class.hash(state)
    }
}

impl MyHash for Box<dyn Type> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl MyHash for Tok {
    fn hash<H: Hasher>(&self, state: &mut H) {
        format!("{}", self).hash(state)
    }
}

#[allow(dead_code)]
pub struct AstHasher<H: Hasher> {
    hasher: H,
}

#[allow(dead_code)]
impl<H: Hasher> AstHasher<H> {
    pub fn new(hasher: H) -> Self {
        Self { hasher }
    }

    pub fn calc_expression(&mut self, expr: &Expression) -> u64 {
        self.visit_expression(expr);
        self.hasher.finish()
    }

    pub fn calc_statement(&mut self, stmt: &Statement) -> u64 {
        self.visit_statement(stmt);
        self.hasher.finish()
    }
}

#[allow(dead_code)]
pub struct Crc32Hasher<'a> {
    digest: RefCell<Option<Digest<'a, u32>>>,
}

impl Crc32Hasher<'_> {
    pub fn new() -> Self {
        Self {
            digest: RefCell::new(Some(CRC32.digest())),
        }
    }
}

impl Hasher for Crc32Hasher<'_> {
    fn finish(&self) -> u64 {
        self.digest.take().unwrap().finalize() as u64
    }

    fn write(&mut self, bytes: &[u8]) {
        self.digest.get_mut().as_mut().unwrap().update(bytes)
    }
}

impl<H: Hasher> AstVisitor<'_> for AstHasher<H> {
    fn visit_literal(&mut self, literal: &LiteralExpression) {
        VisitType::Literal.hash(&mut self.hasher);
        literal.literal().hash(&mut self.hasher)
    }

    fn visit_variable_expression(&mut self, variable: &'_ VariableExpression) {
        VisitType::Variable.hash(&mut self.hasher);
        variable.name().hash(&mut self.hasher)
    }

    fn visit_expr_statement(&mut self, stmt: &ExprStatement) {
        VisitType::ExprStatement.hash(&mut self.hasher);
        self.visit_expression(stmt.expr())
    }

    fn visit_if_statement(&mut self, if_stmt: &IfStatement) {
        VisitType::IfStatement.hash(&mut self.hasher);
        self.visit_expression(if_stmt.condition());

        if let Some(then) = if_stmt.then_controlled() {
            VisitType::ThenStatement.hash(&mut self.hasher);
            self.visit_statement(then);
        }

        for else_if in if_stmt.else_if_list() {
            VisitType::ElseIfStatement.hash(&mut self.hasher);
            self.visit_expression(else_if.condition());
            if let Some(then) = else_if.then_controlled() {
                VisitType::ThenStatement.hash(&mut self.hasher);
                self.visit_statement(then);
            }
        }

        if let Some(else_ctrl) = if_stmt.else_controlled() {
            VisitType::ElseStatement.hash(&mut self.hasher);
            self.visit_statement(else_ctrl)
        }
    }

    fn visit_declaration(&mut self, _: &Declaration) {
        VisitType::DeclarationStatement.hash(&mut self.hasher);
        unimplemented!()
    }

    fn visit_operator_expression(&mut self, op_expr: &OperatorExpression) {
        VisitType::OperatorExpression.hash(&mut self.hasher);
        op_expr.op().hash(&mut self.hasher);
        if let Some(ty) = op_expr.ty() {
            ty.hash(&mut self.hasher);
        }

        op_expr.operands().len().hash(&mut self.hasher);
        for operand in op_expr.operands() {
            VisitType::Operand.hash(&mut self.hasher);
            self.visit_expression(operand);
        }
    }

    fn visit_assign_expression(&mut self, assign_expr: &AssignExpression) {
        VisitType::AssignExpression.hash(&mut self.hasher);
        self.visit_expression(assign_expr.left());
        self.visit_expression(assign_expr.right());
    }

    fn visit_compo_access_expression(&mut self, compo_expr: &CompoAccessExpression) {
        VisitType::CompoAccessExpression.hash(&mut self.hasher);
        self.visit_expression(compo_expr.left());
        self.visit_expression(compo_expr.right());
    }
}
