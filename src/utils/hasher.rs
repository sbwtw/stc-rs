use crate::ast::*;
use crate::parser::{LiteralType, Tok};
use crc::crc32;
use std::hash::{Hash, Hasher};

trait MyHash {
    fn hash<H: Hasher>(&self, state: &mut H);
}

impl MyHash for LiteralType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        format!("{}", self).hash(state)
    }
}

impl MyHash for Variable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name().hash(state);
        self.scope_class().hash(state);
        self.annotation().hash(state);
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

pub struct AstHasher<H: Hasher> {
    hasher: H,
}

impl<H: Hasher> AstHasher<H> {
    pub fn new(hasher: H) -> Self {
        Self { hasher }
    }

    pub fn add(&mut self, ast: &dyn AstNode) {
        ast.accept(self)
    }

    pub fn hash(&self) -> u64 {
        self.hasher.finish()
    }
}

impl AstHasher<crc32::Digest> {
    pub fn crc32() -> Self {
        Self::new(crc32::Digest::new(crc32::IEEE))
    }
}

impl<H: Hasher> AstVisitor for AstHasher<H> {
    fn visit_literal(&mut self, literal: &LiteralType) {
        literal.hash(&mut self.hasher)
    }

    fn visit_variable(&mut self, v: &Variable) {
        v.hash(&mut self.hasher)
    }

    fn visit_statement_list(&mut self, stmts: &StatementList) {
        for s in stmts.statements() {
            s.accept(self)
        }
    }

    fn visit_expr_statement(&mut self, stmt: &ExprStatement) {
        stmt.expr().accept(self)
    }

    fn visit_if_statement(&mut self, if_stmt: &IfStatement) {
        if_stmt.condition().accept(self);

        if let Some(then) = if_stmt.then_controlled() {
            then.accept(self);
        }

        for else_if in if_stmt.else_if_list() {
            else_if.condition().accept(self);
            if let Some(then) = else_if.then_controlled() {
                then.accept(self);
            }
        }

        if let Some(else_ctrl) = if_stmt.else_controlled() {
            else_ctrl.accept(self)
        }
    }

    fn visit_declaration_statement(&mut self, _: &DeclarationStatement) {
        unimplemented!()
    }

    fn visit_operator_expression(&mut self, op_expr: &OperatorExpression) {
        op_expr.op().hash(&mut self.hasher);
        if let Some(ty) = op_expr.ty() {
            ty.hash(&mut self.hasher);
        }

        for operand in op_expr.operands() {
            operand.accept(self);
        }
    }

    fn visit_assign_expression(&mut self, assign_expr: &AssignExpression) {
        assign_expr.left().accept(self);
        assign_expr.right().accept(self);
    }

    fn visit_compo_access_expression(&mut self, compo_expr: &CompoAccessExpression) {
        compo_expr.left().accept(self);
        compo_expr.right().accept(self);
    }
}
