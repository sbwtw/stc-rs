use crate::codegen::*;

pub struct LLVMBackend {
    #[allow(dead_code)]
    adapter: BackendAdapter,
}

impl Backend for LLVMBackend {
    fn gen_function(&mut self) {
        todo!()
    }
}
