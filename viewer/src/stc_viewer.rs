use crate::column_object::ColumnObject;

use async_channel::{Receiver, Sender};
use glib::subclass::prelude::ObjectSubclassIsExt;
use glib::value::ValueTypeMismatchOrNoneError;
use gtk4::glib::Type;
use gtk4::prelude::*;
use gtk4::{Button, SearchEntry, TextBuffer, TextView, TreeStore, TreeView, TreeViewColumn};
use log::info;
use stc::analysis::TypeAnalyzer;
use stc::backend::{CodeGenerator, LuaBackend};
use stc::prelude::*;
use stc::utils::write_ast_to_file;

pub const STC_VIEWER_COLUMN_NAME: u32 = 0;
pub const STC_VIEWER_COLUMN_OBJECT: u32 = 1;

/// Send UI operations from other threads
pub enum UIMessages {
    Refresh,
}

pub struct StcViewerApp {
    pub mgr: UnitsManager,
    pub ui_tx: Sender<UIMessages>,

    pub tree_view: TreeView,
    pub tree_store: TreeStore,
    pub tree_column_name: TreeViewColumn,
    pub tree_column_object: TreeViewColumn,
    pub content_view: TextView,
    pub content_buffer: TextBuffer,
    pub search_entry: SearchEntry,
    pub refresh_button: Button,
    pub compile_button: Button,
    pub run_button: Button,
}

impl StcViewerApp {
    pub fn new(mgr: UnitsManager) -> (Self, Receiver<UIMessages>) {
        let (tx, rx) = async_channel::unbounded();

        let content_buffer = TextBuffer::builder().build();
        let content_view = TextView::with_buffer(&content_buffer);

        let tree_store = TreeStore::new(&[Type::STRING, Type::OBJECT]);
        let tree_view = TreeView::with_model(&tree_store);

        let r = Self {
            mgr,
            ui_tx: tx,

            tree_view,
            tree_store,
            tree_column_name: TreeViewColumn::new(),
            tree_column_object: TreeViewColumn::new(),
            content_view,
            content_buffer,
            search_entry: SearchEntry::new(),
            refresh_button: Button::with_label("Refresh"),
            compile_button: Button::with_label("Compile"),
            run_button: Button::with_label("Run"),
        };

        (r, rx)
    }

    pub fn on_cursor_changed(&self) {
        let (path, _col) = TreeViewExt::cursor(&self.tree_view);

        if let Some(p) = path {
            let iter = self.tree_store.iter(&p);
            match self
                .tree_store
                .get_value(&iter.unwrap(), 1)
                .get::<ColumnObject>()
            {
                Ok(column_object) => {
                    let s = column_object.imp().content();
                    self.content_buffer.set_text(&s);
                }
                Err(ValueTypeMismatchOrNoneError::UnexpectedNone) => {}
                Err(e) => panic!("{:?}", e),
            }
        }

        // dbg!(path.map(|x| x.to_str()), column.map(|x| x.to_string()));
    }

    pub fn refresh(&self) {
        // record last selection
        let (last_tree_path, last_tree_column) = TreeViewExt::cursor(&self.tree_view);
        self.tree_store.clear();

        for ctx in self.mgr.read().contexts() {
            let column_object = ColumnObject::from_ctx(ctx);
            let ctx_iter = self.tree_store.insert_with_values(
                None,
                None,
                &[
                    (STC_VIEWER_COLUMN_NAME, &format!("{}", ctx.read())),
                    (STC_VIEWER_COLUMN_OBJECT, &column_object),
                ],
            );

            // Declarations
            let decl_iter = self.tree_store.insert_with_values(
                Some(&ctx_iter),
                None,
                &[(STC_VIEWER_COLUMN_NAME, &"Declarations")],
            );
            for decl in ctx.read().declarations() {
                let proto = ColumnObject::from_proto(decl);
                self.tree_store.insert_with_values(
                    Some(&decl_iter),
                    None,
                    &[
                        (STC_VIEWER_COLUMN_NAME, &format!("{}", decl.read().unwrap())),
                        (STC_VIEWER_COLUMN_OBJECT, &proto),
                    ],
                );
            }

            // Functions
            let function_iter =
                self.tree_store
                    .insert_with_values(Some(&ctx_iter), None, &[(0, &"Functions")]);
            for fun in ctx.read().functions() {
                let fun_object = ColumnObject::from_func(fun);
                let fun_id = fun.read().decl_id();
                let decl_name = ctx
                    .read()
                    .get_declaration_by_id(fun_id)
                    .map(|x| x.read().unwrap().to_string());

                self.tree_store.insert_with_values(
                    Some(&function_iter),
                    None,
                    &[
                        (
                            STC_VIEWER_COLUMN_NAME,
                            &decl_name
                                .unwrap_or(format!("No Name({})", fun_id))
                                .to_string(),
                        ),
                        (STC_VIEWER_COLUMN_OBJECT, &fun_object),
                    ],
                );
            }
        }

        // let tx = self.ui_tx.clone();
        // gio::spawn_blocking(move || {
        //     let five_seconds = Duration::from_secs(5);
        //     thread::sleep(five_seconds);
        //
        //     glib::idle_add(move || {
        //         tx.send(UIMessages::ResetTreeViewPath).unwrap();
        //         ControlFlow::Break
        //     });
        // });

        // reset to last selected row
        if let Some(p) = last_tree_path {
            self.tree_view.expand_to_path(&p);
            TreeViewExt::set_cursor(&self.tree_view, &p, last_tree_column.as_ref(), false);

            // refresh contents
            self.on_cursor_changed()
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
                let mut f = f.write();

                let scope = Scope::new(Some(self.mgr.clone()), Some(app_id), Some(proto_id));
                type_analyzer.analyze_statement(f.parse_tree_mut(), scope);

                write_ast_to_file(f.parse_tree(), proto_read.name());

                info!("{}\n{}", proto_read, f.parse_tree());
            }
        }

        let mut code_gen: CodeGenerator<LuaBackend> =
            CodeGenerator::new(self.mgr.clone(), app_id).unwrap();
        println!("CodeGen: {:?}", code_gen.build_application());
    }

    pub fn run(&mut self) {}
}
