use crate::ast::*;
use crate::backend::CompiledCode;
use crate::context::task::TaskInfo;
use crate::context::ModuleKind;
use crate::parser::StString;
use indexmap::IndexMap;
use log::warn;
use once_cell::sync::Lazy;
use smallvec::smallvec;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};
use uuid::Uuid;

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

    fn with_object_id(decl: Declaration, id: Uuid) -> Self {
        Self {
            inner: Arc::new(RwLock::new(PrototypeImpl::with_object_id(decl, id))),
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

    fn with_prototype(proto: &Prototype, function: Statement) -> Self {
        Self {
            inner: Arc::new(RwLock::new(FunctionImpl::with_prototype(proto, function))),
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
    object_id: Uuid,
    decl: Declaration,
}

impl PrototypeImpl {
    fn new(decl: Declaration) -> Self {
        Self {
            id: get_next_declaration_id(),
            object_id: Uuid::nil(),
            decl,
        }
    }

    fn with_object_id(decl: Declaration, id: Uuid) -> Self {
        Self {
            id: get_next_declaration_id(),
            object_id: id,
            decl,
        }
    }

    pub fn object_id(&self) -> Uuid {
        self.object_id
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

    pub fn variables(&self) -> &[Arc<Variable>] {
        self.decl.variables()
    }

    pub fn create_user_type(&self) -> Option<Type> {
        match self.decl.kind {
            DeclKind::Struct(_) => Some(StructType::new(self.name().clone(), self.id).into()),
            _ => None,
        }
    }

    pub fn set_decl(&mut self, decl: Declaration) {
        self.decl = decl
    }

    /// Get return value of prototype
    pub fn return_value(&self) -> Option<&Arc<Variable>> {
        self.variables().iter().find(|x| x.name() == self.name())
    }

    /// return false if Prototype is Function, like FB or Fun or Prg
    #[inline]
    pub fn is_type_declaration(&self) -> bool {
        matches!(
            self.decl.kind,
            DeclKind::GlobalVar(_) | DeclKind::Alias(_) | DeclKind::Enum(_)
        )
    }
}

fn proto_name_string(name: &StString) -> Cow<String> {
    let s = name.string();

    if s.is_empty() {
        return Cow::Owned("(No Name)".to_owned());
    }

    Cow::Borrowed(name.string())
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
    object_id: Uuid,
    parse_tree: Statement,
    compiled_code: Option<Box<dyn CompiledCode>>,
}

impl FunctionImpl {
    fn new(decl_id: usize, function: Statement) -> Self {
        Self {
            decl_id,
            object_id: Uuid::nil(),
            parse_tree: function,
            compiled_code: None,
        }
    }

    fn with_prototype(proto: &Prototype, function: Statement) -> Self {
        let proto = proto.read().unwrap();

        Self {
            decl_id: proto.id,
            object_id: proto.object_id(),
            parse_tree: function,
            compiled_code: None,
        }
    }

    #[inline]
    pub fn object_id(&self) -> Uuid {
        self.object_id
    }

    #[inline]
    pub fn decl_id(&self) -> usize {
        self.decl_id
    }

    #[inline]
    pub fn parse_tree(&self) -> &Statement {
        &self.parse_tree
    }

    #[inline]
    pub fn parse_tree_mut(&mut self) -> &mut Statement {
        &mut self.parse_tree
    }

    #[inline]
    pub fn compiled_code(&self) -> &Option<Box<dyn CompiledCode>> {
        &self.compiled_code
    }

    #[inline]
    pub fn set_compiled_code(&mut self, compiled_code: Box<dyn CompiledCode>) {
        self.compiled_code = Some(compiled_code)
    }
}

#[derive(Clone)]
pub struct ModuleContext {
    inner: Arc<RwLock<ModuleContextImpl>>,
}

impl PartialEq for ModuleContext {
    fn eq(&self, other: &Self) -> bool {
        self.inner.read().unwrap().id == other.inner.read().unwrap().id
    }
}

impl Eq for ModuleContext {}

impl ModuleContext {
    pub fn new(kind: ModuleKind) -> Self {
        Self {
            inner: Arc::new(RwLock::new(ModuleContextImpl {
                id: get_next_context_id(),
                kind,
                declaration_id_map: IndexMap::new(),
                declaration_uuid_map: HashMap::new(),
                declaration_name_map: HashMap::new(),
                function_id_map: IndexMap::new(),
                toplevel_global_variable_declarations: HashSet::new(),
                task_info: smallvec![],
            })),
        }
    }

    #[inline]
    pub fn read(&self) -> RwLockReadGuard<'_, ModuleContextImpl> {
        self.inner.read().unwrap()
    }

    #[inline]
    pub fn write(&self) -> RwLockWriteGuard<'_, ModuleContextImpl> {
        self.inner.write().unwrap()
    }
}

pub struct ModuleContextImpl {
    id: usize,
    kind: ModuleKind,
    declaration_id_map: IndexMap<usize, Prototype>,
    declaration_uuid_map: HashMap<Uuid, Prototype>,
    declaration_name_map: HashMap<StString, Prototype>,
    function_id_map: IndexMap<usize, Function>,
    toplevel_global_variable_declarations: HashSet<Prototype>,
    task_info: SmallVec8<TaskInfo>,
}

impl ModuleContextImpl {
    #[inline]
    pub fn id(&self) -> usize {
        self.id
    }

    #[inline]
    pub fn scope(&self) -> &ModuleKind {
        &self.kind
    }

    #[inline]
    pub fn is_app_ctx(&self) -> bool {
        matches!(self.kind, ModuleKind::Application)
    }

    pub fn add_declaration(&mut self, decl: Declaration, id: Uuid) -> usize {
        // update decl
        if let Some(proto) = self.declaration_uuid_map.get_mut(&id) {
            proto.write().unwrap().set_decl(decl);
            return proto.read().unwrap().id();
        }

        let name = decl.identifier().clone();
        let mut toplevel_global_variable_declaration = false;

        if let DeclKind::GlobalVar(ref g) = decl.kind {
            if g.name().is_empty() {
                toplevel_global_variable_declaration = true;
            }
        }

        let decl = Prototype::with_object_id(decl, id);
        let proto_id = decl.read().unwrap().id;

        self.declaration_id_map.insert(proto_id, decl.clone());
        self.declaration_name_map.insert(name, decl.clone());
        self.declaration_uuid_map.insert(id, decl.clone());

        if toplevel_global_variable_declaration {
            self.toplevel_global_variable_declarations.insert(decl);
        }

        proto_id
    }

    /// Returns old value if exists
    pub fn add_function(&mut self, decl_id: usize, fun: Statement) -> Option<Function> {
        let fun = match self.get_declaration_by_id(decl_id) {
            Some(decl) => Function::with_prototype(decl, fun),
            _ => {
                warn!("Declaration of function {decl_id} not found");
                Function::new(decl_id, fun)
            }
        };

        self.function_id_map.insert(decl_id, fun)
    }

    pub fn add_function_with_proto(
        &mut self,
        proto: &Prototype,
        fun: Statement,
    ) -> Option<Function> {
        let decl_id = proto.read().unwrap().id;
        let fun = Function::with_prototype(proto, fun);

        self.function_id_map.insert(decl_id, fun)
    }

    #[inline]
    pub fn declarations(&self) -> impl Iterator<Item = &Prototype> {
        self.declaration_id_map.values()
    }

    #[inline]
    pub fn declaration_ids(&self) -> impl Iterator<Item = &usize> {
        self.declaration_id_map.keys()
    }

    #[inline]
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.function_id_map.values()
    }

    #[inline]
    pub fn get_function(&self, decl_id: usize) -> Option<&Function> {
        self.function_id_map.get(&decl_id)
    }

    #[inline]
    pub fn get_declaration_by_id(&self, decl_id: usize) -> Option<&Prototype> {
        self.declaration_id_map.get(&decl_id)
    }

    #[inline]
    pub fn get_declaration_by_uuid(&self, uuid: &Uuid) -> Option<&Prototype> {
        self.declaration_uuid_map.get(uuid)
    }

    #[inline]
    pub fn find_declaration_by_name(&self, ident: &StString) -> Option<&Prototype> {
        self.declaration_name_map.get(ident)
    }

    #[inline]
    pub fn find_toplevel_global_variable(&self, ident: &StString) -> Option<Arc<Variable>> {
        self.find_toplevel_global_variable_map(|x| x.name() == ident)
    }

    pub fn find_toplevel_global_variable_map<F>(&self, f: F) -> Option<Arc<Variable>>
    where
        F: Fn(&Arc<Variable>) -> bool,
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
        let scope = match self.kind {
            ModuleKind::Application => "App",
            ModuleKind::CompilerBuiltin => "Builtin",
            ModuleKind::Library => "Library",
        };

        f.write_fmt(format_args!("{}_{}", scope, self.id))
    }
}
