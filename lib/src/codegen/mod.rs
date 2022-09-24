pub mod llvm;
pub use llvm::LLVMBackend;

pub mod backend_adapter;
pub use backend_adapter::*;

pub trait Backend {
    fn gen_function(&mut self);
}
