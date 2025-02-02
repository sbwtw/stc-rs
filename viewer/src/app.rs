use stc::analysis::TypeAnalyzer;
use stc::backend::{CodeGenBackend, CodeGenDriver, LLVMBackend, LuaBackend};
use stc::prelude::*;
use stc::serde::Project;
use stc::utils::write_ast_to_file;

use crate::egui::CompileBackend;
use log::info;
use quick_xml::de::from_str;
use std::fs;
use std::io::Write;

#[derive(Default)]
pub(crate) struct StcViewerApp {
    pub(crate) mgr: UnitsManager,
}

impl StcViewerApp {
    #[inline]
    pub fn with_mgr(mgr: UnitsManager) -> Self {
        Self { mgr }
    }

    #[inline]
    pub fn from_app(app: Project) -> Self {
        let mgr = UnitsManager::new();
        let ctx: ModuleContext = app.into();
        mgr.write().add_context(ctx.clone());
        mgr.write().set_active_application(Some(ctx.read().id()));

        Self::with_mgr(mgr)
    }

    #[inline]
    #[cfg(debug_assertions)]
    pub fn load_test_project() -> Self {
        let app = from_str(include_str!("../test_projects/example1/test_proj.xml"));
        Self::from_app(app.unwrap())
    }

    pub fn compile(&mut self, backend: CompileBackend) {
        let app = self.mgr.read().active_application().unwrap();
        let app_read = app.read();
        let app_id = app_read.id();

        let mut type_analyzer = TypeAnalyzer::new();
        for proto in app_read.declarations() {
            let proto_read = proto.read().unwrap();
            let proto_id = proto_read.id();
            let fun = app_read.get_function(proto_read.id());

            if let Some(f) = fun {
                let mut f = f.write();

                let scope = Scope::new(Some(self.mgr.clone()), Some(app_id), Some(proto_id));
                type_analyzer.analyze_statement(f.parse_tree_mut(), scope);

                write_ast_to_file(f.parse_tree(), proto_read.name());

                info!("{}\n{}", proto_read, f.parse_tree());
            }
        }

        match backend {
            CompileBackend::Lua => self.generate_code_lua(app_id),
            CompileBackend::LLVM => self.generate_code_llvm(app_id),
        }
    }

    fn generate_code_llvm(&mut self, app_id: usize) {
        let mut code_gen: CodeGenDriver<LLVMBackend> =
            CodeGenDriver::new(self.mgr.clone(), app_id).unwrap();
        println!("CodeGen: {:?}", code_gen.build_application());
    }

    fn generate_code_lua(&mut self, app_id: usize) {
        let mut code_gen: CodeGenDriver<LuaBackend> =
            CodeGenDriver::new(self.mgr.clone(), app_id).unwrap();
        println!("CodeGen: {:?}", code_gen.build_application());

        let mut buf = vec![0u8; 0];
        code_gen.backend().get_module_bytes(&mut buf).unwrap();

        let mut f = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open("/home/stc.o")
            .unwrap();
        f.write_all(&buf).unwrap();
    }
}
