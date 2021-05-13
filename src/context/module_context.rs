use crate::ast::*;
use crate::context::ModuleContextScope;
use crate::parser::StString;
use once_cell::sync::Lazy;
use std::cell::{Cell, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, RwLock};

static CONTEXT_ID: Lazy<AtomicUsize> = Lazy::new(|| AtomicUsize::new(0));
static DECLARATION_ID: Lazy<AtomicUsize> = Lazy::new(|| AtomicUsize::new(0));

fn get_next_context_id() -> usize {
    CONTEXT_ID.fetch_add(1, Ordering::SeqCst)
}

fn get_next_declaration_id() -> usize {
    DECLARATION_ID.fetch_add(1, Ordering::SeqCst)
}

#[derive(Clone)]
struct DeclarationWrapper {
    id: usize,
    decl: Arc<RwLock<Box<dyn Declaration>>>,
}

impl DeclarationWrapper {
    fn new(decl: Box<dyn Declaration>) -> Self {
        Self {
            id: get_next_declaration_id(),
            decl: Arc::new(RwLock::new(decl)),
        }
    }
}

#[derive(Clone)]
struct FunctionWrapper {
    decl_id: usize,
    function: Arc<RwLock<Box<dyn Statement>>>,
}

impl PartialEq for ModuleContext {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for ModuleContext {}

pub struct ModuleContext {
    id: usize,
    scope: ModuleContextScope,
    declaration_id_map: HashMap<usize, DeclarationWrapper>,
    declaration_name_map: HashMap<StString, DeclarationWrapper>,
}

impl ModuleContext {
    pub fn new(scope: ModuleContextScope) -> Self {
        Self {
            id: get_next_context_id(),
            scope,
            declaration_id_map: HashMap::new(),
            declaration_name_map: HashMap::new(),
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn scope(&self) -> &ModuleContextScope {
        &self.scope
    }

    pub fn add_declaration(&mut self, decl: Box<dyn Declaration>) {
        let name = decl.identifier().clone();
        let wrapper = DeclarationWrapper::new(decl);

        self.declaration_id_map.insert(wrapper.id, wrapper.clone());
        self.declaration_name_map.insert(name, wrapper.clone());
    }

    pub fn get_declaration_by_id(
        &self,
        decl_id: usize,
    ) -> Option<Arc<RwLock<Box<dyn Declaration>>>> {
        self.declaration_id_map
            .get(&decl_id)
            .map(|x| x.decl.clone())
    }
}
