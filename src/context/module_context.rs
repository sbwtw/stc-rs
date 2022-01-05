use crate::ast::*;
use crate::context::ModuleContextScope;
use crate::parser::StString;
use once_cell::sync::Lazy;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
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
pub struct Prototype {
    inner: Arc<RwLock<PrototypeImpl>>,
}

impl PartialEq for Prototype {
    fn eq(&self, other: &Self) -> bool {
        self.read().unwrap().id == other.read().unwrap().id
    }
}

impl Eq for Prototype {}

impl Hash for Prototype {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.read().unwrap().id)
    }
}

impl Deref for Prototype {
    type Target = Arc<RwLock<PrototypeImpl>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl Prototype {
    fn new(decl: Declaration) -> Self {
        Self {
            inner: Arc::new(RwLock::new(PrototypeImpl::new(decl))),
        }
    }
}

#[derive(Clone)]
pub struct Function {
    inner: Arc<RwLock<FunctionImpl>>,
}

impl Deref for Function {
    type Target = Arc<RwLock<FunctionImpl>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl Function {
    fn new(decl_id: usize, function: Statement) -> Self {
        Self {
            inner: Arc::new(RwLock::new(FunctionImpl::new(decl_id, function))),
        }
    }
}

pub struct PrototypeImpl {
    id: usize,
    decl: Declaration,
}

impl PrototypeImpl {
    fn new(decl: Declaration) -> Self {
        Self {
            id: get_next_declaration_id(),
            decl,
        }
    }

    pub fn variables(&self) -> Cow<Vec<Rc<Variable>>> {
        self.decl.variables()
    }

    pub fn ty(&self) -> Option<Rc<Box<dyn Type>>> {
        None
    }
}

pub struct FunctionImpl {
    decl_id: usize,
    function: Statement,
}

impl FunctionImpl {
    fn new(decl_id: usize, function: Statement) -> Self {
        Self { decl_id, function }
    }

    pub fn body(&self) -> &Statement {
        &self.function
    }

    pub fn body_mut(&mut self) -> &mut Statement {
        &mut self.function
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

        let decl = Prototype::new(decl);
        let proto_id = decl.read().unwrap().id;

        self.declaration_id_map.insert(proto_id, decl.clone());
        self.declaration_name_map.insert(name, decl.clone());

        if toplevel_global_variable_declaration {
            self.toplevel_global_variable_declarations
                .insert(decl.clone());
        }

        proto_id
    }

    pub fn add_function(&mut self, decl_id: usize, fun: Statement) -> Option<Function> {
        let fun = Function::new(decl_id, fun);

        self.function_id_map.insert(decl_id, fun).map(|x| x.clone())
    }

    pub fn declarations(&self) -> impl Iterator<Item = &Prototype> {
        self.declaration_id_map.values().into_iter()
    }

    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.function_id_map.values().into_iter()
    }

    pub fn get_function(&self, decl_id: usize) -> Option<Function> {
        self.function_id_map.get(&decl_id).map(|x| x.clone())
    }

    pub fn get_declaration_by_id(&self, decl_id: usize) -> Option<Prototype> {
        self.declaration_id_map.get(&decl_id).map(|x| x.clone())
    }

    pub fn find_declaration_by_name(&self, ident: &StString) -> Option<Prototype> {
        self.declaration_name_map.get(ident).map(|x| x.clone())
    }

    pub fn find_toplevel_global_variable(&self, ident: &StString) -> Option<Rc<Variable>> {
        self.find_toplevel_global_variable_map(|x| x.name() == ident)
    }

    pub fn find_toplevel_global_variable_map<F>(&self, f: F) -> Option<Rc<Variable>>
    where
        F: Fn(&Rc<Variable>) -> bool,
    {
        for decl in self.toplevel_global_variable_declarations.iter() {
            let decl = decl.read().unwrap();
            for v in decl.decl.variables().iter() {
                if f(v) {
                    return Some(v.clone());
                }
            }
        }

        return None;
    }
}
