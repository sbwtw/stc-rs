use crate::ast::Variable;
use crate::context::{ModuleContext, Prototype, UnitsManager};
use crate::parser::StString;
use std::rc::Rc;

#[derive(Clone, Default)]
#[allow(unused)]
pub struct Scope {
    units_manager: Option<UnitsManager>,
    local_context: Option<ModuleContext>,
    local_declaration: Option<Prototype>,
}

impl Scope {
    pub fn new(
        mgr: Option<UnitsManager>,
        local_ctx: Option<usize>,
        local_function: Option<usize>,
    ) -> Self {
        let ctx = mgr
            .as_ref()
            .zip(local_ctx)
            .and_then(|(mgr, ctx_id)| mgr.read().get_context(ctx_id));
        let local_declaration = ctx
            .as_ref()
            .zip(local_function)
            .and_then(|(ctx, local_id)| ctx.read().get_declaration_by_id(local_id).cloned());

        Self {
            units_manager: mgr,
            local_context: ctx,
            local_declaration,
        }
    }

    pub fn find_declaration(&self, ident: &StString) -> (Option<Prototype>, Option<Scope>) {
        let decl = self
            .local_context
            .as_ref()
            .and_then(|ctx| ctx.read().find_declaration_by_name(ident).cloned());

        match decl {
            None => (None, None),
            Some(decl) => {
                let ctx_id = self.local_context.as_ref().map(|x| x.read().id());
                let fun_id = decl.read().unwrap().id();
                (
                    Some(decl),
                    Some(Self::new(self.units_manager.clone(), ctx_id, Some(fun_id))),
                )
            }
        }
    }

    pub fn find_variable(&self, ident: &StString) -> Option<Rc<Variable>> {
        self.find_local_variable(ident)
            .or_else(|| self.find_global_variable(ident))
    }

    pub fn find_local_variable(&self, ident: &StString) -> Option<Rc<Variable>> {
        self.local_declaration.as_ref().and_then(|decl| {
            let decl = decl.read().unwrap();
            decl.variables().iter().find(|x| x.name() == ident).cloned()
        })
    }

    pub fn find_global_variable(&self, ident: &StString) -> Option<Rc<Variable>> {
        self.local_context
            .as_ref()
            .and_then(|ctx| ctx.read().find_toplevel_global_variable(ident))
    }
}
