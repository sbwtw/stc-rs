use gtk::builders::TextBufferBuilder;
use gtk::glib::Type;
use gtk::prelude::*;
use gtk::{Button, SearchEntry, TextBuffer, TextView, TreeStore, TreeView, TreeViewColumn};
use stc::context::UnitsManager;

#[derive(Clone)]
pub struct StcViewerApp {
    pub mgr: UnitsManager,

    pub tree_view: TreeView,
    pub tree_model: TreeStore,
    pub tree_column_name: TreeViewColumn,
    pub content_view: TextView,
    pub content_buffer: TextBuffer,
    pub search_entry: SearchEntry,
    pub refresh_button: Button,
}

impl StcViewerApp {
    pub fn new(mgr: UnitsManager) -> Self {
        let content_buffer = TextBufferBuilder::new().build();
        let content_view = TextView::with_buffer(&content_buffer);

        let tree_model = TreeStore::new(&[Type::STRING]);
        let tree_view = TreeView::with_model(&tree_model);

        Self {
            mgr,

            tree_view,
            tree_model,
            tree_column_name: TreeViewColumn::new(),
            content_view,
            content_buffer,
            search_entry: SearchEntry::new(),
            refresh_button: Button::with_label("Refresh"),
        }
    }

    pub fn refresh(&self) {
        self.tree_model.clear();

        for ctx in self.mgr.read().contexts() {
            let ctx_iter =
                self.tree_model
                    .insert_with_values(None, None, &[(0, &format!("{}", ctx.read()))]);

            // Declarations
            let decl_iter =
                self.tree_model
                    .insert_with_values(Some(&ctx_iter), None, &[(0, &"Declarations")]);
            for decl in ctx.read().declarations() {
                self.tree_model.insert_with_values(
                    Some(&decl_iter),
                    None,
                    &[(0, &format!("{}", decl.read().unwrap()))],
                );
            }

            // Functions
            let function_iter =
                self.tree_model
                    .insert_with_values(Some(&ctx_iter), None, &[(0, &"Functions")]);
            for fun in ctx.read().functions() {
                self.tree_model.insert_with_values(
                    Some(&function_iter),
                    None,
                    &[(0, &format!("{}", fun.read().unwrap().decl_id()))],
                );
            }
        }
    }
}
