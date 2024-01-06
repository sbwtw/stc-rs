mod column_object;
mod stc_viewer;
mod storage;

use crate::stc_viewer::{StcViewerApp, UIMessages, STC_VIEWER_COLUMN_NAME};
use glib::MainContext;
use gtk::prelude::*;
use gtk::{
    Adjustment, Application, ApplicationWindow, CellRendererText, Orientation, Paned,
    ScrolledWindow, WindowPosition, WrapMode,
};
use quick_xml::de::from_str;
use stc::prelude::*;
use std::rc::Rc;
use std::sync::Mutex;

fn main() {
    pretty_env_logger::init();

    let mgr = UnitsManager::new();
    let proj: Result<storage::Application, _> =
        from_str(include_str!("../test_projects/example1/test_proj.xml"));
    let ctx: ModuleContext = proj.unwrap().into();
    mgr.write().add_context(ctx.clone());
    mgr.write().set_active_application(Some(ctx.read().id()));

    let gtk_app = Application::new(None, Default::default());
    gtk_app.connect_activate(move |app| build_ui(app, mgr.clone()));
    gtk_app.run();
}

fn build_ui(app: &Application, mgr: UnitsManager) {
    let (stc_app, rx) = StcViewerApp::new(mgr);
    let window = ApplicationWindow::new(app);

    window.set_title("STC compilation units viewer");
    window.set_position(WindowPosition::Center);
    window.set_size_request(800, 600);

    stc_app.content_view.set_monospace(true);
    stc_app.content_view.set_wrap_mode(WrapMode::WordChar);

    let cell = CellRendererText::new();
    CellLayoutExt::pack_start(&stc_app.tree_column_name, &cell, true);
    TreeViewColumnExt::add_attribute(
        &stc_app.tree_column_name,
        &cell,
        "text",
        STC_VIEWER_COLUMN_NAME as i32,
    );

    // let cell = CellRendererText::new();
    // CellLayoutExt::pack_start(&stc_app.tree_column_object, &cell, true);
    // TreeViewColumnExt::add_attribute(
    //     &stc_app.tree_column_object,
    //     &cell,
    //     "text",
    //     STC_VIEWER_COLUMN_OBJECT as i32,
    // );

    stc_app.tree_view.append_column(&stc_app.tree_column_name);
    stc_app.tree_view.append_column(&stc_app.tree_column_object);
    stc_app.tree_view.set_headers_visible(false);

    let tree_scroll = ScrolledWindow::new(Adjustment::NONE, Adjustment::NONE);
    tree_scroll.add(&stc_app.tree_view);
    tree_scroll.set_expand(true);

    let left_layout = gtk::Box::new(Orientation::Vertical, 0);
    left_layout.add(&stc_app.search_entry);
    left_layout.add(&tree_scroll);
    left_layout.set_width_request(250);

    let content_scroll = ScrolledWindow::new(Adjustment::NONE, Adjustment::NONE);
    content_scroll.add(&stc_app.content_view);
    content_scroll.set_expand(true);

    let button_layout = gtk::Box::new(Orientation::Horizontal, 0);
    button_layout.add(&stc_app.refresh_button);
    button_layout.add(&stc_app.compile_button);

    let right_layout = gtk::Box::new(Orientation::Vertical, 0);
    right_layout.add(&button_layout);
    right_layout.add(&content_scroll);

    let paned = Paned::new(Orientation::Horizontal);
    paned.add(&left_layout);
    paned.add(&right_layout);

    let stc_app = Rc::new(Mutex::new(stc_app));
    let app_copy = stc_app.clone();
    let app_lock = stc_app.lock().unwrap();
    app_lock
        .refresh_button
        .connect_clicked(move |_| app_copy.lock().unwrap().refresh());

    let app_copy = stc_app.clone();
    app_lock
        .compile_button
        .connect_clicked(move |_| app_copy.lock().unwrap().compile());

    let app_copy = stc_app.clone();
    app_lock.tree_view.connect_cursor_changed(move |_| {
        if let Ok(app) = app_copy.try_lock() {
            app.on_cursor_changed()
        }
    });

    // refresh UI when the window is shown
    let tx = app_lock.ui_tx.clone();
    window.connect_show(move |_| {
        let tx = tx.clone();

        glib::idle_add_once(move || {
            tx.send_blocking(UIMessages::Refresh).unwrap();
        });
    });

    // handle UI messages from other threads
    let app_copy = stc_app.clone();
    let ctx = MainContext::default();
    ctx.spawn_local(async move {
        while let Ok(r) = rx.recv().await {
            match r {
                UIMessages::Refresh => app_copy.lock().unwrap().refresh(),
            }
        }
    });

    window.add(&paned);
    window.show_all();
}
