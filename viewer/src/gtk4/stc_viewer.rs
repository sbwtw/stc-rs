use super::column_object::ColumnObject;

use crate::app::StcViewerApp;
use crate::PrototypeDisplayName;

use async_channel::{Receiver, Sender};
use glib::subclass::prelude::ObjectSubclassIsExt;
use glib::value::ValueTypeMismatchOrNoneError;
use gtk4::gio::Menu;
use gtk4::glib::Type;
use gtk4::prelude::*;
use gtk4::{Button, SearchEntry, TextBuffer, TextView, TreeStore, TreeView, TreeViewColumn};

pub const STC_VIEWER_COLUMN_NAME: u32 = 0;
pub const STC_VIEWER_COLUMN_OBJECT: u32 = 1;

/// Send UI operations from other threads
pub enum UIMessages {
    Refresh,
}

pub struct StcViewerGtk4 {
    pub app: StcViewerApp,
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

impl StcViewerGtk4 {
    pub fn new(app: StcViewerApp) -> (Self, Receiver<UIMessages>) {
        let (tx, rx) = async_channel::unbounded();

        let content_buffer = TextBuffer::builder().build();
        let content_view = TextView::with_buffer(&content_buffer);

        let tree_store = TreeStore::new(&[Type::STRING, Type::OBJECT]);
        let tree_view = TreeView::with_model(&tree_store);

        let r = Self {
            app,
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

    pub fn popup_menu_model(&self) -> Menu {
        let m = Menu::new();
        m.append(Some("a"), None);
        m.append(Some("b"), None);

        m
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

        for ctx in self.app.mgr.read().contexts() {
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
                        (STC_VIEWER_COLUMN_NAME, &decl.display_name()),
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
                    .unwrap()
                    .display_name();

                self.tree_store.insert_with_values(
                    Some(&function_iter),
                    None,
                    &[
                        (STC_VIEWER_COLUMN_NAME, &decl_name),
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

    pub fn run(&mut self) {}
}
