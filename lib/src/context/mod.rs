mod module_context;
pub use module_context::{Function, ModuleContext, Prototype};

mod units_manager;
pub use units_manager::UnitsManager;

mod scope;
mod task;
mod library;

pub use scope::Scope;

pub enum ModuleKind {
    Application,
    CompilerBuiltin,
    Library,
    // System,
}

pub trait Library {}
