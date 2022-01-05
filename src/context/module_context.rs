use crate::ast::*;
use crate::context::ModuleContextScope;
use crate::parser::StString;
use once_cell::sync::Lazy;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
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
#[allow(unused)]
pub struct Prototype {
    id: usize,
    decl: Arc<RwLock<Declaration>>,
}

impl Prototype {
    fn new(decl: Declaration) -> Self {
        Self {
            id: get_next_declaration_id(),
            decl: Arc::new(RwLock::new(decl)),
        }
    }
}

impl PartialEq for Prototype {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Prototype {}

impl Hash for Prototype {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.id)
    }
}

#[derive(Clone)]
#[allow(unused)]
pub struct Function {
    decl_id: usize,
    function: Arc<RwLock<Statement>>,
}

impl Function {
    fn new(decl_id: usize, function: Arc<RwLock<Statement>>) -> Self {
        Self { decl_id, function }
    }
}

impl PartialEq for ModuleContext {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for ModuleContext {}

#[allow(unused)]
pub struct ModuleContext {
    id: usize,
    scope: ModuleContextScope,
    declaration_id_map: HashMap<usize, Prototype>,
    declaration_name_map: HashMap<StString, Prototype>,
    function_id_map: HashMap<usize, Function>,
    toplevel_global_variable_declarations: HashSet<Prototype>,
}

impl ModuleContext {
    pub fn new(scope: ModuleContextScope) -> Self {
        Self {
            id: get_next_context_id(),
            scope,
            declaration_id_map: HashMap::new(),
            declaration_name_map: HashMap::new(),
            function_id_map: HashMap::new(),
            toplevel_global_variable_declarations: HashSet::new(),
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    #[allow(dead_code)]
    pub fn scope(&self) -> &ModuleContextScope {
        &self.scope
    }

    pub fn add_declaration(&mut self, decl: Declaration) -> usize {
        let name = decl.identifier().clone();
        let mut toplevel_global_variable_declaration = false;

        if let DeclKind::GlobalVar(ref g) = decl.kind {
            if g.name().is_empty() {
                toplevel_global_variable_declaration = true;
            }
        }

        let wrapper = Prototype::new(decl);

        self.declaration_id_map.insert(wrapper.id, wrapper.clone());
        self.declaration_name_map.insert(name, wrapper.clone());

        if toplevel_global_variable_declaration {
            self.toplevel_global_variable_declarations
                .insert(wrapper.clone());
        }

        wrapper.id
    }

    pub fn add_function(
        &mut self,
        decl_id: usize,
        fun: Statement,
    ) -> Option<Arc<RwLock<Statement>>> {
        self.function_id_map
            .insert(decl_id, Function::new(decl_id, Arc::new(RwLock::new(fun))))
            .map(|x| x.function.clone())
    }

    pub fn declarations(&self) -> impl Iterator<Item = &Arc<RwLock<Declaration>>> {
        self.declaration_id_map
            .values()
            .map(|x| &x.decl)
            .into_iter()
    }

    pub fn functions(&self) -> impl Iterator<Item = &Arc<RwLock<Statement>>> {
        self.function_id_map
            .values()
            .map(|x| &x.function)
            .into_iter()
    }

    pub fn get_function(&self, decl_id: usize) -> Option<Arc<RwLock<Statement>>> {
        self.function_id_map
            .get(&decl_id)
            .map(|x| x.function.clone())
    }

    pub fn get_declaration_by_id(&self, decl_id: usize) -> Option<Arc<RwLock<Declaration>>> {
        self.declaration_id_map
            .get(&decl_id)
            .map(|x| x.decl.clone())
    }

    pub fn find_declaration_by_name(&self, ident: &StString) -> Option<Arc<RwLock<Declaration>>> {
        self.declaration_name_map.get(ident).map(|x| x.decl.clone())
    }

    pub fn find_toplevel_global_variable(&self, ident: &StString) -> Option<Rc<Variable>> {
        self.find_toplevel_global_variable_map(|x| x.name() == ident)
    }

    pub fn find_toplevel_global_variable_map<F>(&self, f: F) -> Option<Rc<Variable>>
    where
        F: Fn(&Rc<Variable>) -> bool,
    {
        for decl in self.toplevel_global_variable_declarations.iter() {
            let decl = decl.decl.read().unwrap();
            for v in decl.variables().iter() {
                if f(v) {
                    return Some(v.clone());
                }
            }
        }

        return None;
    }
}
