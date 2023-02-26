mod code_adapter;

use crate::ast::AstVisitorMut;
use crate::codegen::code_adapter::BackendAdapterImpl;
use crate::context::{ModuleContext, UnitsManager};
use log::info;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

pub trait BackendAdapter: AstVisitorMut {
    fn setup(&mut self, app: ModuleContext);
    fn generate(&mut self, fun_id: usize);
}

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

pub struct CodeGenerator<T: BackendAdapter> {
    mgr: UnitsManager,
    adapter: T,
}

impl CodeGenerator<BackendAdapterImpl> {
    pub fn new(mgr: UnitsManager) -> Self {
        Self {
            mgr: mgr.clone(),
            adapter: BackendAdapterImpl::new(mgr),
        }
    }
}

impl<T: BackendAdapter> CodeGenerator<T> {
    pub fn build_application(&mut self, app_id: usize) -> Result<(), CodeGenError> {
        // find the App
        let app = self
            .mgr
            .read()
            .get_context(app_id)
            .ok_or(CodeGenError::AppNotFound)?;
        self.adapter.setup(app.clone());

        let mut fun_ids: Vec<_> = app.read().declaration_ids().copied().collect();
        fun_ids.sort();

        for f in fun_ids {
            info!("generating code for function {}", f);

            self.adapter.generate(f);
        }

        Ok(())
    }
}
