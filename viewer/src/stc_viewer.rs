use gtk::builders::TextBufferBuilder;
use gtk::glib::Type;
use gtk::{Button, SearchEntry, TextBuffer, TextView, TreeStore, TreeView};

#[derive(Clone)]
pub struct StcViewerApp {
    pub tree_view: TreeView,
    pub tree_model: TreeStore,
    pub content_view: TextView,
    pub content_buffer: TextBuffer,
    pub search_entry: SearchEntry,
    pub refresh_button: Button,
}

impl StcViewerApp {
    pub fn new() -> Self {
        let content_buffer = TextBufferBuilder::new().build();
        let content_view = TextView::with_buffer(&content_buffer);

        let tree_model = TreeStore::new(&[Type::STRING]);
        let tree_view = TreeView::with_model(&tree_model);

        Self {
            tree_view,
            tree_model,
            content_view,
            content_buffer,
            search_entry: SearchEntry::new(),
            refresh_button: Button::with_label("Refresh"),
        }
    }

    pub fn refresh(&self) {
        println!("refresh data!")
    }
}
