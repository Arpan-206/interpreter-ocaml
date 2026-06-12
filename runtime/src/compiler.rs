// compiler.rs — LLVM AOT compiler via inkwell

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};

use crate::ast::{Expr, Stmt};

pub struct LoxCompiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> LoxCompiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Result<Self, String> {
        let module = context.create_module("lox");
        let builder = context.create_builder();
        Ok(LoxCompiler {
            context,
            module,
            builder,
        })
    }

    pub fn compile_program(&mut self, stmts: Vec<Stmt>) -> Result<(), String> {
        // TODO — emit LLVM IR for each statement
        Ok(())
    }

    pub fn write_object(&self, output_path: &str) -> Result<(), String> {
        Target::initialize_aarch64(&InitializationConfig::default());

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).map_err(|e| e.to_string())?;
        let machine = target
            .create_target_machine(
                &triple,
                "apple-m1",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or("failed to create target machine")?;

        machine
            .write_to_file(&self.module, FileType::Object, output_path.as_ref())
            .map_err(|e| e.to_string())
    }
}

pub fn compile(bytes: &[u8], output_path: &str) -> Result<(), String> {
    let stmts: Vec<crate::ast::Stmt> =
        bincode::deserialize(bytes).map_err(|e| format!("deserialisation failed: {}", e))?;

    let context = Context::create();
    let mut compiler = LoxCompiler::new(&context)?;
    compiler.compile_program(stmts)?;
    compiler.write_object(output_path)
}
