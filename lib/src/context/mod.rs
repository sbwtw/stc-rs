mod module_context;
pub use module_context::{Function, ModuleContext, Prototype};

mod units_manager;
pub use units_manager::UnitsManager;

mod scope;
mod task;

pub use scope::Scope;

pub enum ModuleContextScope {
    Application,
    CompilerBuiltin,
    Library,
    // System,
}
