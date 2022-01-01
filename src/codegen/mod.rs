pub mod arm;
pub use arm::ArmBackend;

pub mod backend_adapter;
pub use backend_adapter::*;

pub trait Backend {
    fn gen_function(&mut self);
}
