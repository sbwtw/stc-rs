use crate::ast::*;
use crate::context::{ModuleContext, UnitsManager};
use crate::parser::StString;
use std::sync::{Arc, RwLock};

#[derive(Clone)]
pub struct Scope {
    units_manager: Option<Arc<RwLock<UnitsManager>>>,
    local_context: Option<Arc<RwLock<ModuleContext>>>,
    local_declaration: Option<Arc<RwLock<Box<dyn Declaration>>>>,
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

    pub fn find_variable(&self, ident: &StString) -> Option<Arc<Variable>> {
        self.local_declaration.as_ref().and_then(|decl| {
            decl.read()
                .unwrap()
                .as_any()
                .downcast_ref::<FunctionDeclaration>()
                .and_then(|fun| {
                    fun.variables()
                        .iter()
                        .find(|x| x.name() == ident)
                        .map(|x| x.clone())
                })
        })
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
