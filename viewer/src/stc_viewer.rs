use gtk::glib::Type;
use gtk::prelude::*;
use gtk::{Button, SearchEntry, TextBuffer, TextView, TreeStore, TreeView, TreeViewColumn};
use log::info;
use stc::analysis::TypeAnalyzer;
use stc::codegen::CodeGenerator;
use stc::context::{Scope, UnitsManager};
use stc::utils::write_ast_to_file;

pub const STC_VIEWER_COLUMN_NAME: u32 = 0;

pub struct StcViewerApp {
    pub mgr: UnitsManager,

    pub tree_view: TreeView,
    pub tree_store: TreeStore,
    pub tree_column_name: TreeViewColumn,
    pub tree_column_data: TreeViewColumn,
    pub content_view: TextView,
    pub content_buffer: TextBuffer,
    pub search_entry: SearchEntry,
    pub refresh_button: Button,
    pub compile_button: Button,
}

impl StcViewerApp {
    pub fn new(mgr: UnitsManager) -> Self {
        let content_buffer = TextBuffer::builder().build();
        let content_view = TextView::with_buffer(&content_buffer);

        let tree_store = TreeStore::new(&[Type::STRING]);
        let tree_view = TreeView::with_model(&tree_store);

        Self {
            mgr,

            tree_view,
            tree_store,
            tree_column_name: TreeViewColumn::new(),
            tree_column_data: TreeViewColumn::new(),
            content_view,
            content_buffer,
            search_entry: SearchEntry::new(),
            refresh_button: Button::with_label("Refresh"),
            compile_button: Button::with_label("Compile"),
        }
    }

    pub fn on_cursor_changed(&self) {
        let (path, column) = self.tree_view.cursor();

        // if let Some(p) = path {
        //     let iter = self.tree_store.iter(&p);
        //     // let x = iter.unwrap().to_value().get::<PrototypeNode>();
        //     dbg!(iter, p);
        // }V

        dbg!(path.map(|x| x.to_str()), column.map(|x| x.to_string()));
    }

    pub fn refresh(&self) {
        self.tree_store.clear();

        for ctx in self.mgr.read().contexts() {
            let ctx_iter = self.tree_store.insert_with_values(
                None,
                None,
                &[(STC_VIEWER_COLUMN_NAME, &format!("{}", ctx.read()))],
            );

            // Declarations
            let decl_iter = self.tree_store.insert_with_values(
                Some(&ctx_iter),
                None,
                &[(STC_VIEWER_COLUMN_NAME, &"Declarations")],
            );
            for decl in ctx.read().declarations() {
                self.tree_store.insert_with_values(
                    Some(&decl_iter),
                    None,
                    &[(STC_VIEWER_COLUMN_NAME, &format!("{}", decl.read().unwrap()))],
                );
            }

            // Functions
            let function_iter =
                self.tree_store
                    .insert_with_values(Some(&ctx_iter), None, &[(0, &"Functions")]);
            for fun in ctx.read().functions() {
                self.tree_store.insert_with_values(
                    Some(&function_iter),
                    None,
                    &[(
                        STC_VIEWER_COLUMN_NAME,
                        &format!("{}", fun.read().unwrap().decl_id()),
                    )],
                );
            }
        }
    }

    pub fn compile(&self) {
        let app = self.mgr.read().active_application().unwrap();
        let app_read = app.read();
        let app_id = app_read.id();

        let mut type_analyzer = TypeAnalyzer::new();
        for proto in app_read.declarations() {
            let proto_read = proto.read().unwrap();
            let proto_id = proto_read.id();
            let fun = app_read.get_function(proto_read.id());

            if let Some(f) = fun {
                let mut f = f.write().unwrap();

                let scope = Scope::new(Some(self.mgr.clone()), Some(app_id), Some(proto_id));
                type_analyzer.analyze_statement(f.body_mut(), scope);

                write_ast_to_file(f.body(), proto_read.name().origin_string());

                info!("{}\n{}", proto_read, f.body());
            }
        }

        let mut code_gen = CodeGenerator::new(self.mgr.clone(), app_id).unwrap();
        println!("CodeGen: {:?}", code_gen.build_application());
    }
}

// struct TreeProtoNode {
//     proto: Prototype,
// }
