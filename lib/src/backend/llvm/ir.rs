use crate::prelude::AstVisitor;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::OptimizationLevel;

type TestFunc = unsafe extern "C" fn(u64, u64) -> u64;

pub struct LLVMBackendContext {
    pub(crate) llvm_ctx: Context,
}

impl LLVMBackendContext {
    pub fn new() -> Self {
        Self {
            llvm_ctx: Context::create(),
        }
    }
}

pub struct LLVMModuleBuilder<'ctx> {
    context: &'ctx LLVMBackendContext,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> LLVMModuleBuilder<'ctx> {
    pub fn new(ctx: &'ctx LLVMBackendContext) -> Self {
        let module = ctx.llvm_ctx.create_module("main");
        let builder = ctx.llvm_ctx.create_builder();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        Self {
            context: ctx,
            module,
            builder,
            execution_engine,
        }
    }

    pub fn test(&self) {
        let i64_type = self.llvm_ctx().i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.llvm_ctx().append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0).unwrap().into_int_value();
        let y = function.get_nth_param(1).unwrap().into_int_value();
        let sum = self.builder.build_int_add(x, y, "sum").unwrap();

        self.builder.build_return(Some(&sum)).unwrap();

        let r = unsafe {
            let func: JitFunction<TestFunc> = self.execution_engine.get_function("main").unwrap();
            func.call(1, 2)
        };
        println!("Result: {}", r);
    }

    #[inline]
    fn llvm_ctx(&self) -> &'ctx Context {
        &self.context.llvm_ctx
    }
}

impl AstVisitor<'_> for LLVMModuleBuilder<'_> {}
