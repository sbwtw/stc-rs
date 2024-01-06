use crate::ast::*;
use crate::backend::TargetCode;
use crate::context::ModuleContextScope;
use crate::parser::StString;
use indexmap::IndexMap;
use once_cell::sync::Lazy;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

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
    inner: Rc<RwLock<PrototypeImpl>>,
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
    type Target = Rc<RwLock<PrototypeImpl>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl Prototype {
    fn new(decl: Declaration) -> Self {
        Self {
            inner: Rc::new(RwLock::new(PrototypeImpl::new(decl))),
        }
    }
}

#[derive(Clone)]
pub struct Function {
    inner: Rc<RwLock<FunctionImpl>>,
}

impl Deref for Function {
    type Target = Rc<RwLock<FunctionImpl>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl Function {
    fn new(decl_id: usize, function: Statement) -> Self {
        Self {
            inner: Rc::new(RwLock::new(FunctionImpl::new(decl_id, function))),
        }
    }

    pub fn read(&self) -> RwLockReadGuard<'_, FunctionImpl> {
        self.inner.read().unwrap()
    }

    pub fn write(&self) -> RwLockWriteGuard<'_, FunctionImpl> {
        self.inner.write().unwrap()
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

    pub fn decl(&self) -> &Declaration {
        &self.decl
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn name(&self) -> &StString {
        self.decl.identifier()
    }

    pub fn variables(&self) -> &[Rc<Variable>] {
        self.decl.variables()
    }

    pub fn create_user_type(&self) -> Option<Rc<Box<dyn Type>>> {
        // Only Structure types can be created as UserType
        match self.decl.kind {
            DeclKind::Struct(_) | DeclKind::Alias(_) | DeclKind::Enum(_) => {}
            _ => return None,
        }

        let user_ty = UserType::from_proto(self.decl.identifier().clone(), self.id);
        Some(Rc::new(Box::new(user_ty)))
    }

    /// Get return value of prototype
    pub fn return_value(&self) -> Option<&Rc<Variable>> {
        self.variables().iter().find(|x| x.name() == self.name())
    }

    /// return false if Prototype is Function, like FB or Fun or Prg
    pub fn is_type_declaration(&self) -> bool {
        matches!(
            self.decl.kind,
            DeclKind::GlobalVar(_) | DeclKind::Alias(_) | DeclKind::Enum(_)
        )
    }
}

fn proto_name_string(name: &StString) -> Cow<String> {
    let s = name.origin_string();

    if s.is_empty() {
        return Cow::Owned("(No Name)".to_owned());
    }

    Cow::Borrowed(name.origin_string())
}

impl Display for PrototypeImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.decl.kind {
            DeclKind::FB(fun) => f.write_fmt(format_args!(
                "{} ({})",
                proto_name_string(fun.name()),
                "FUNCTION_BLOCK"
            )),
            DeclKind::Prg(fun) => f.write_fmt(format_args!(
                "{} ({})",
                proto_name_string(fun.name()),
                "PROGRAM"
            )),
            DeclKind::Fun(fun) => f.write_fmt(format_args!(
                "{} ({})",
                proto_name_string(fun.name()),
                "FUN"
            )),
            DeclKind::Alias(alias) => f.write_fmt(format_args!(
                "{} ({})",
                proto_name_string(alias.name()),
                "TYPE"
            )),
            DeclKind::Struct(s) => f.write_fmt(format_args!(
                "{} ({})",
                proto_name_string(s.name()),
                "STRUCT"
            )),
            DeclKind::Enum(e) => {
                f.write_fmt(format_args!("{} ({})", proto_name_string(e.name()), "ENUM"))
            }
            DeclKind::GlobalVar(g) => f.write_fmt(format_args!(
                "{} ({})",
                proto_name_string(g.name()),
                "VAR_GLOBAL"
            )),
        }
    }
}

pub struct FunctionImpl {
    decl_id: usize,
    parse_tree: Statement,
    compiled_code: Option<Box<dyn TargetCode>>,
}

impl FunctionImpl {
    fn new(decl_id: usize, function: Statement) -> Self {
        Self {
            decl_id,
            parse_tree: function,
            compiled_code: None,
        }
    }

    pub fn decl_id(&self) -> usize {
        self.decl_id
    }

    pub fn parse_tree(&self) -> &Statement {
        &self.parse_tree
    }

    pub fn parse_tree_mut(&mut self) -> &mut Statement {
        &mut self.parse_tree
    }

    pub fn compiled_code(&self) -> &Option<Box<dyn TargetCode>> {
        &self.compiled_code
    }

    pub fn set_compiled_code(&mut self, compiled_code: Box<dyn TargetCode>) {
        self.compiled_code = Some(compiled_code)
    }
}

#[derive(Clone)]
pub struct ModuleContext {
    inner: Rc<RwLock<ModuleContextImpl>>,
}

impl PartialEq for ModuleContext {
    fn eq(&self, other: &Self) -> bool {
        self.inner.read().unwrap().id == other.inner.read().unwrap().id
    }
}

impl Eq for ModuleContext {}

impl ModuleContext {
    pub fn new(scope: ModuleContextScope) -> Self {
        Self {
            inner: Rc::new(RwLock::new(ModuleContextImpl {
                id: get_next_context_id(),
                scope,
                declaration_id_map: IndexMap::new(),
                declaration_name_map: HashMap::new(),
                function_id_map: IndexMap::new(),
                toplevel_global_variable_declarations: HashSet::new(),
            })),
        }
    }

    pub fn read(&self) -> RwLockReadGuard<'_, ModuleContextImpl> {
        self.inner.read().unwrap()
    }

    pub fn write(&self) -> RwLockWriteGuard<'_, ModuleContextImpl> {
        self.inner.write().unwrap()
    }
}

pub struct ModuleContextImpl {
    id: usize,
    scope: ModuleContextScope,
    declaration_id_map: IndexMap<usize, Prototype>,
    declaration_name_map: HashMap<StString, Prototype>,
    function_id_map: IndexMap<usize, Function>,
    toplevel_global_variable_declarations: HashSet<Prototype>,
}

impl ModuleContextImpl {
    pub fn id(&self) -> usize {
        self.id
    }

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
            self.toplevel_global_variable_declarations.insert(decl);
        }

        proto_id
    }

    pub fn add_function(&mut self, decl_id: usize, fun: Statement) -> Option<Function> {
        let fun = Function::new(decl_id, fun);

        self.function_id_map.insert(decl_id, fun)
    }

    pub fn declarations(&self) -> impl Iterator<Item = &Prototype> {
        self.declaration_id_map.values()
    }

    pub fn declaration_ids(&self) -> impl Iterator<Item = &usize> {
        self.declaration_id_map.keys()
    }

    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.function_id_map.values()
    }

    pub fn get_function(&self, decl_id: usize) -> Option<&Function> {
        self.function_id_map.get(&decl_id)
    }

    pub fn get_declaration_by_id(&self, decl_id: usize) -> Option<&Prototype> {
        self.declaration_id_map.get(&decl_id)
    }

    pub fn find_declaration_by_name(&self, ident: &StString) -> Option<&Prototype> {
        self.declaration_name_map.get(ident)
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

        None
    }
}

impl Display for ModuleContextImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let scope = match self.scope {
            ModuleContextScope::Application => "App",
            ModuleContextScope::CompilerBuiltin => "Builtin",
            ModuleContextScope::Library => "Library",
        };

        f.write_fmt(format_args!("{}_{}", scope, self.id))
    }
}
