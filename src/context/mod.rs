mod module_context;
pub use module_context::ModuleContext;

mod units_manager;
pub use units_manager::UnitsManager;

mod scope;
pub use scope::Scope;

pub enum ModuleContextScope {
    Application,
    Library,
    System,
}
