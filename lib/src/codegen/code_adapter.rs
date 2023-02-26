use crate::ast::{AstVisitorMut, IfStatement};
use crate::codegen::BackendAdapter;
use crate::context::{ModuleContext, UnitsManager};

use log::trace;
use std::ops::Deref;

pub struct BackendAdapterImpl {
    mgr: UnitsManager,
    app: Option<ModuleContext>,
}

impl BackendAdapterImpl {
    pub fn new(mgr: UnitsManager) -> Self {
        Self { mgr, app: None }
    }
}

impl BackendAdapter for BackendAdapterImpl {
    fn setup(&mut self, app: ModuleContext) {
        self.app = Some(app);
    }

    fn generate(&mut self, fun_id: usize) {
        let app_ctx = self.app.as_ref().unwrap().clone();
        let proto = app_ctx
            .read()
            .get_declaration_by_id(fun_id)
            .unwrap()
            .clone();
        trace!("proto: {}", proto.read().unwrap());

        let fun = app_ctx.read().get_function(fun_id).unwrap().clone();
        let mut fun = fun.write().unwrap();

        self.visit_statement_mut(fun.body_mut());
    }
}

impl AstVisitorMut for BackendAdapterImpl {
    fn visit_if_statement_mut(&mut self, ifst: &mut IfStatement) {
        trace!("visit_if_statement_mut");
    }
}
