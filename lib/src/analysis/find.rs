use crate::prelude::*;
use bitflags::bitflags;

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct HintRole: u8 {
        const EXPRESSION         = 0b0000_0001;
        const STATEMENT          = 0b0000_0010;
        const ACCESS_PATH        = 0b0000_0100;
    }
}

pub fn find_access_path(stmt: &Statement, pos: &TokLoc) -> Option<Expression> {
    let finder = AstFinder {
        hint_role: HintRole::ACCESS_PATH,
        find_result: None,
        hint_target: pos,
    };
    finder.find_expression(stmt)
}

struct AstFinder<'a> {
    hint_role: HintRole,
    hint_target: &'a TokLoc,
    find_result: Option<Expression>,
}

impl AstFinder<'_> {
    pub fn find_expression(mut self, stmt: &Statement) -> Option<Expression> {
        self.visit_statement(stmt);
        self.find_result
    }

    pub fn try_hint<T>(&mut self, expr: &Spanned<T>)
    where
        T: Clone,
        Spanned<T>: IntoExpression,
    {
        if self.find_result.is_none() && expr.hint(self.hint_target) {
            self.find_result = Some(expr.clone().into_expression())
        }
    }
}

impl AstVisitor<'_> for AstFinder<'_> {
    fn visit_variable_expression(&mut self, variable: &'_ Spanned<VariableExpression>) {
        if self.hint_role.contains(HintRole::ACCESS_PATH) {
            self.try_hint(variable);
        }
    }

    fn visit_statement_list(&mut self, stmts: &'_ Vec<Statement>) {
        // TODO: binary search?
        for stmt in stmts {
            self.visit_statement(stmt);

            if self.find_result.is_some() {
                break;
            }
        }
    }
}
