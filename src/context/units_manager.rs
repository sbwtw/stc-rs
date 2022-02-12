use crate::context::ModuleContext;
use smallmap::Map;

/// Program Organization Units Manager
#[allow(dead_code)]
pub struct UnitsManager {
    active_application: Option<usize>,
    contexts: Map<usize, ModuleContext>,
}

#[allow(dead_code)]
impl UnitsManager {
    pub fn new() -> Self {
        Self {
            active_application: None,
            contexts: Map::new(),
        }
    }

    pub fn add_context(&mut self, ctx: ModuleContext) {
        let id = ctx.read().id();
        if self.contexts.contains_key(&id) {
            return;
        }

        self.contexts.insert(id, ctx.clone());
    }

    pub fn set_active_application(&mut self, app: Option<usize>) {
        self.active_application = app
    }

    pub fn get_context(&self, ctx_id: usize) -> Option<ModuleContext> {
        self.contexts.get(&ctx_id).map(|x| x.clone())
    }
}
