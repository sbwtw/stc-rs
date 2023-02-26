use crate::context::ModuleContext;
use smallmap::Map;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

/// Program Organization Units Manager
#[allow(dead_code)]
#[derive(Clone)]
pub struct UnitsManager {
    inner: Arc<RwLock<UnitsManagerImpl>>,
}

impl UnitsManager {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn read(&self) -> RwLockReadGuard<'_, UnitsManagerImpl> {
        self.inner.read().unwrap()
    }

    pub fn write(&self) -> RwLockWriteGuard<'_, UnitsManagerImpl> {
        self.inner.write().unwrap()
    }
}

impl Default for UnitsManager {
    fn default() -> Self {
        let mgr = UnitsManagerImpl::new();

        Self {
            inner: Arc::new(RwLock::new(mgr)),
        }
    }
}

#[allow(dead_code)]
pub struct UnitsManagerImpl {
    active_application: Option<usize>,
    contexts: Map<usize, ModuleContext>,
}

#[allow(dead_code)]
impl UnitsManagerImpl {
    fn new() -> Self {
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

        self.contexts.insert(id, ctx);
    }

    pub fn set_active_application(&mut self, app: Option<usize>) {
        self.active_application = app
    }

    pub fn get_context(&self, ctx_id: usize) -> Option<ModuleContext> {
        self.contexts.get(&ctx_id).cloned()
    }

    pub fn active_application(&self) -> Option<ModuleContext> {
        self.active_application.and_then(|x| self.get_context(x))
    }

    pub fn contexts(&self) -> impl Iterator<Item = &ModuleContext> {
        self.contexts.values()
    }
}
