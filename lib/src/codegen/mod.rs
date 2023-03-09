mod code_adapter;
mod lua_backend;

use crate::ast::{AstVisitorMut, IfStatement, OperatorExpression, Variable};
use crate::codegen::code_adapter::CodeGenAdapterImpl;
use crate::context::{ModuleContext, UnitsManager};

use crate::codegen::lua_backend::LuaBackend;
use log::info;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

pub trait CodeGenAdapter: AstVisitorMut {
    fn generate(&mut self, fun_id: usize) -> Box<dyn TargetCode>;
}

pub trait CodeGenBackend {
    type Label;

    fn take_code(&mut self) -> Box<dyn TargetCode>;
    fn define_label<S: AsRef<str>>(&mut self, label: Option<S>) -> Self::Label;
    fn gen_variable_load(&mut self, variable: &mut Variable);
    fn gen_operator(&mut self, operator: &mut OperatorExpression);
}

pub trait TargetCode: Display {}

pub enum CodeGenError {
    AppNotFound,
}

impl Error for CodeGenError {}

impl Display for CodeGenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CodeGenError::AppNotFound => f.write_str("Application Not Found"),
        }
    }
}

impl Debug for CodeGenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

pub struct CodeGenerator<T: CodeGenAdapter> {
    mgr: UnitsManager,
    app: ModuleContext,
    adapter: T,
}

impl CodeGenerator<CodeGenAdapterImpl<LuaBackend>> {
    pub fn new(mgr: UnitsManager, app_id: usize) -> Result<Self, CodeGenError> {
        let app = mgr
            .read()
            .get_context(app_id)
            .ok_or(CodeGenError::AppNotFound)?;

        Ok(Self {
            mgr: mgr.clone(),
            app: app.clone(),
            adapter: CodeGenAdapterImpl::new(mgr, app, LuaBackend::new()),
        })
    }
}

impl<T: CodeGenAdapter> CodeGenerator<T> {
    pub fn build_application(&mut self) -> Result<(), CodeGenError> {
        let mut fun_ids: Vec<_> = self.app.read().declaration_ids().copied().collect();
        fun_ids.sort();

        for f in fun_ids {
            info!("generating code for function {}", f);

            self.adapter.generate(f);
        }

        Ok(())
    }
}
