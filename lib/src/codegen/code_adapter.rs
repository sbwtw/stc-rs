use crate::ast::{AcceptMut, AstVisitorMut, Expression, IfStatement, VariableExpression};
use crate::codegen::{CodeGenAdapter, CodeGenBackend, TargetCode};
use crate::context::{ModuleContext, UnitsManager};

use log::trace;

trait CodeGenVisitor: AstVisitorMut {}

pub struct CodeGenAdapterImpl<B: CodeGenBackend> {
    mgr: UnitsManager,
    app: ModuleContext,
    backend: B,
}

impl<T: CodeGenBackend> CodeGenAdapterImpl<T> {
    pub fn new(mgr: UnitsManager, app: ModuleContext, backend: T) -> Self {
        Self { mgr, app, backend }
    }
}

impl<T: CodeGenBackend> CodeGenAdapter for CodeGenAdapterImpl<T> {
    fn generate(&mut self, fun_id: usize) -> Box<dyn TargetCode> {
        let app_ctx = self.app.clone();
        let proto = app_ctx
            .read()
            .get_declaration_by_id(fun_id)
            .unwrap()
            .clone();
        trace!("proto: {}", proto.read().unwrap());

        let fun = app_ctx.read().get_function(fun_id).unwrap().clone();
        let mut fun = fun.write().unwrap();

        self.visit_statement_mut(fun.body_mut());
        self.backend.take_code()
    }
}

impl<T: CodeGenBackend> AstVisitorMut for CodeGenAdapterImpl<T> {
    fn visit_expression_mut(&mut self, expr: &mut Expression) {}

    fn visit_if_statement_mut(&mut self, ifst: &mut IfStatement) {
        let condition_false = self.backend.define_label(Some("condition_false"));

        self.visit_expression_mut(ifst.condition_mut())
    }
}
