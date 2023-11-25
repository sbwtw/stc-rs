mod lua_backend;

use crate::ast::{OperatorExpression, Variable};
use crate::codegen::lua_backend::LuaBackend;
use crate::context::{ModuleContext, UnitsManager};

use bitflags::bitflags;
use log::info;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct AccessModeFlags: u32 {
        const NONE              = 0b0000_0000_0000_0000;
        const READ              = 0b0000_0000_0000_0001;
        const WRITE             = 0b0000_0000_0000_0010;
        const PARAMETER         = 0b0000_0000_0000_0100;
    }
}

pub trait CodeGenBackend {
    type Label;

    fn take_code(&mut self) -> Box<dyn TargetCode>;
    fn define_label<S: AsRef<str>>(&mut self, label: Option<S>) -> Self::Label;
    fn gen_function(&mut self, func: usize) -> Result<(), CodeGenError>;
    fn gen_variable_load(&mut self, variable: &mut Variable);
    fn gen_operator(&mut self, operator: &mut OperatorExpression);
}

pub trait TargetCode: Display {}

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
    backend: B,
}

impl CodeGenerator<LuaBackend> {
    pub fn new(mgr: UnitsManager, app_id: usize) -> Result<Self, CodeGenError> {
        let app = mgr
            .read()
            .get_context(app_id)
            .ok_or(CodeGenError::AppNotFound)?;

        Ok(Self {
            mgr: mgr.clone(),
            app: app.clone(),
            backend: LuaBackend::new(mgr, app),
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
                self.backend.gen_function(decl_id)?;
            }
        }

        Ok(())
    }
}
