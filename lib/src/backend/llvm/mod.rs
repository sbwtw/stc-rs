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
        todo!()
    }

    fn gen_function(self, func: usize) -> Result<Box<dyn TargetCode>, CodeGenError> {
        todo!()
    }

    fn define_label<S: AsRef<str>>(&mut self, label: Option<S>) -> Self::Label {
        todo!()
    }

    fn gen_variable_load(&mut self, variable: &mut Variable) {
        todo!()
    }

    fn gen_operator(&mut self, operator: &mut OperatorExpression) {
        todo!()
    }
}
