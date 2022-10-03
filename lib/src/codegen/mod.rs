use crate::context::UnitsManager;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

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

pub struct CodeGenAdapter {
    mgr: UnitsManager,
}

impl CodeGenAdapter {
    pub fn with_units_manager(mgr: UnitsManager) -> Self {
        Self { mgr }
    }

    pub fn build_application(&self, app_id: usize) -> Result<(), CodeGenError> {
        // find the App
        let _app = self
            .mgr
            .read()
            .get_context(app_id)
            .ok_or(CodeGenError::AppNotFound)?;
        Ok(())
    }
}
