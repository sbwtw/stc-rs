use crate::ast::*;
use crate::context::{ModuleContext, ModuleContextScope};
use crate::parser::StString;
use once_cell::sync::Lazy;
use std::cell::RefMut;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, RwLock};

/// Program Organization Units Manager
pub struct UnitsManager {
    active_application: Option<usize>,
    contexts: HashMap<usize, Arc<RwLock<ModuleContext>>>,
}

impl UnitsManager {
    pub fn new() -> Self {
        Self {
            active_application: None,
            contexts: HashMap::new(),
        }
    }

    pub fn add_context(&mut self, ctx: Arc<RwLock<ModuleContext>>) {
        let id = ctx.read().unwrap().id();
        if self.contexts.contains_key(&id) {
            return;
        }

        self.contexts.insert(id, ctx.clone());
    }

    pub fn set_active_application(&mut self, app: Option<usize>) {
        self.active_application = app
    }

    pub fn get_context(&self, ctx_id: usize) -> Option<Arc<RwLock<ModuleContext>>> {
        self.contexts.get(&ctx_id).map(|x| x.clone())
    }
}
