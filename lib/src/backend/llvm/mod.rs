use crate::backend::*;
use crate::prelude::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

struct LLVMBackendImpl<'ctx> {
    ctx: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

pub struct LLVMBackend {}

impl CodeGenBackend for LLVMBackend {
    type Label = ();

    fn new(mgr: UnitsManager, app: ModuleContext) -> Self {
        Self {}
    }

    fn gen_function(&mut self, func: usize) -> Result<Box<dyn CompiledCode>, CodeGenError> {
        todo!()
    }

    fn create_label<S: AsRef<str>>(&mut self, label: S) -> Self::Label {
        unreachable!()
    }

    fn insert_label(&mut self, label: Self::Label) {
        unreachable!()
    }

    fn gen_variable_load(&mut self, variable: &mut Variable) {
        todo!()
    }

    fn gen_operator(&mut self, operator: &mut OperatorExpression) {
        todo!()
    }

    fn get_module_bytes(&mut self, w: &mut dyn Write) -> io::Result<()> {
        todo!()
    }
}
