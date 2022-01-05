use crate::ast::*;
use crate::context::{ModuleContext, UnitsManager};
use crate::parser::StString;
use std::rc::Rc;
use std::sync::{Arc, RwLock};

#[derive(Clone)]
#[allow(unused)]
pub struct Scope {
    units_manager: Option<Arc<RwLock<UnitsManager>>>,
    local_context: Option<Arc<RwLock<ModuleContext>>>,
    local_declaration: Option<Arc<RwLock<Declaration>>>,
}

impl Scope {
    pub fn new(
        mgr: Option<Arc<RwLock<UnitsManager>>>,
        local_ctx: Option<usize>,
        local_function: Option<usize>,
    ) -> Self {
        let ctx = mgr
            .as_ref()
            .zip(local_ctx)
            .and_then(|(mgr, ctx_id)| mgr.read().unwrap().get_context(ctx_id));
        let local_declaration = ctx
            .as_ref()
            .zip(local_function)
            .and_then(|(ctx, local_id)| {
                ctx.read().unwrap().get_declaration_by_id(local_id).clone()
            });

        Self {
            units_manager: mgr,
            local_context: ctx,
            local_declaration,
        }
    }

    pub fn find_variable(&self, ident: &StString) -> Option<Rc<Variable>> {
        self.find_local_variable(ident)
            .or_else(|| self.find_global_variable(ident))
    }

    pub fn find_local_variable(&self, ident: &StString) -> Option<Rc<Variable>> {
        self.local_declaration.as_ref().and_then(|decl| {
            let decl = decl.read().unwrap();
            decl.variables()
                .iter()
                .find(|x| x.name() == ident)
                .map(|x| x.clone())
        })
    }

    pub fn find_global_variable(&self, ident: &StString) -> Option<Rc<Variable>> {
        self.local_context
            .as_ref()
            .and_then(|ctx| ctx.read().unwrap().find_toplevel_global_variable(ident))
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            units_manager: None,
            local_context: None,
            local_declaration: None,
        }
    }
}
