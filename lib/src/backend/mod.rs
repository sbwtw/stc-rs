#[cfg(feature = "llvm_backend")]
mod llvm;
mod lua;

pub use lua::dump::lua_dump_function;
pub use lua::LuaBackend;

use crate::ast::{OperatorExpression, Variable};
use crate::context::{ModuleContext, UnitsManager};

use bitflags::bitflags;
use log::info;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::io;
use std::io::Write;
use std::marker::PhantomData;

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct AccessModeFlags: u32 {
        const NONE              = 0b0000_0000_0000_0000;
        const READ              = 0b0000_0000_0000_0001;
        const WRITE             = 0b0000_0000_0000_0010;
        const PARAMETER         = 0b0000_0000_0000_0100;
        const CALL              = 0b0000_0000_0000_1000;
    }
}

pub trait CodeGenBackend {
    type Label;

    fn new(mgr: UnitsManager, app: ModuleContext) -> Self;
    fn gen_function(self, func: usize) -> Result<Box<dyn CompiledCode>, CodeGenError>;
    fn define_label<S: AsRef<str>>(&mut self, label: Option<S>) -> Self::Label;
    fn gen_variable_load(&mut self, variable: &mut Variable);
    fn gen_operator(&mut self, operator: &mut OperatorExpression);
}

pub trait CompiledCode: Display {
    fn dump(&self, w: &mut dyn Write) -> io::Result<()>;
}

pub enum CodeGenError {
    AppNotFound,
    FunctionNotDefined(usize),
}

impl Error for CodeGenError {}

impl Display for CodeGenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CodeGenError::AppNotFound => f.write_str("Application Not Found"),
            CodeGenError::FunctionNotDefined(func) => {
                f.write_str(&format!("Function {} not defined", func))
            }
        }
    }
}

impl Debug for CodeGenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

pub struct CodeGenerator<B>
where
    B: CodeGenBackend,
{
    mgr: UnitsManager,
    app: ModuleContext,
    _backend: PhantomData<B>,
}

impl<B> CodeGenerator<B>
where
    B: CodeGenBackend,
{
    pub fn new(mgr: UnitsManager, app_id: usize) -> Result<Self, CodeGenError> {
        let app = mgr
            .read()
            .get_context(app_id)
            .ok_or(CodeGenError::AppNotFound)?;

        Ok(Self {
            mgr: mgr.clone(),
            app: app.clone(),
            _backend: PhantomData,
        })
    }
}

impl<B> CodeGenerator<B>
where
    B: CodeGenBackend,
{
    pub fn build_application(&mut self) -> Result<(), CodeGenError> {
        let mut decl_info: Vec<(_, _)> = self
            .app
            .read()
            .declarations()
            .map(|x| (x.read().unwrap().id(), x.clone()))
            .collect();
        decl_info.sort_by_key(|(x, _)| *x);

        for (decl_id, proto) in decl_info {
            let proto = proto.read().unwrap();
            if !proto.is_type_declaration() {
                info!("generating code for function {} {}", decl_id, proto);

                let f = self
                    .app
                    .read()
                    .get_function(decl_id)
                    .ok_or(CodeGenError::FunctionNotDefined(decl_id))?
                    .clone();

                let backend = B::new(self.mgr.clone(), self.app.clone());
                let target_code = backend.gen_function(decl_id)?;
                println!("{}", target_code);
                f.write().set_compiled_code(target_code);
            }
        }

        Ok(())
    }
}
