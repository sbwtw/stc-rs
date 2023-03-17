mod stc_viewer;

use crate::stc_viewer::{StcViewerApp, STC_VIEWER_COLUMN_NAME};

use stc::context::{ModuleContext, ModuleContextScope, UnitsManager};
use stc::parser::{StDeclarationParser, StFunctionParser, StLexer};

use gtk::prelude::*;
use gtk::{
    Adjustment, Application, ApplicationWindow, CellRendererText, Orientation, Paned,
    ScrolledWindow, WindowPosition, WrapMode,
};
use std::rc::Rc;
use std::sync::Mutex;

fn main() {
    pretty_env_logger::init();

    let mgr = UnitsManager::new();
    let mgr_ui_app = mgr.clone();
    let app = ModuleContext::new(ModuleContextScope::Application);
    let app_id = app.read().id();
    mgr.write().add_context(app);
    mgr.write().set_active_application(Some(app_id));

    let app_ctx = mgr.write().get_context(app_id).unwrap();
    let decl = StLexer::new("function test: int VAR a: INT; b: INT; END_VAR end_function");
    let decl = StDeclarationParser::new().parse(decl).unwrap();
    let decl_id = app_ctx.write().add_declaration(decl);

    let prg = StLexer::new("program prg: int VAR a: BYTE; END_VAR end_program");
    let prg = StDeclarationParser::new().parse(prg).unwrap();
    let _prg_id = app_ctx.write().add_declaration(prg);

    let global = StLexer::new("VAR_GLOBAL END_VAR VAR_GLOBAL 全局变量1: REAL; END_VAR");
    let global = StDeclarationParser::new().parse(global).unwrap();
    let _global_id = app_ctx.write().add_declaration(global);

    let body = StLexer::new("if a < 全局变量1 then prg.a := 1; else b := 2; end_if");
    let body = StFunctionParser::new().parse(body).unwrap();
    app_ctx.write().add_function(decl_id, body);

    let gtk_app = Application::new(None, Default::default());
    gtk_app.connect_activate(move |app| build_ui(app, mgr_ui_app.clone()));
    gtk_app.run();
}

fn build_ui(app: &Application, mgr: UnitsManager) {
    let stc_app = StcViewerApp::new(mgr);
    let window = ApplicationWindow::new(app);

    window.set_title("STC compilation units viewer");
    window.set_position(WindowPosition::Center);
    window.set_size_request(800, 600);

    stc_app.content_view.set_monospace(true);
    stc_app.content_view.set_wrap_mode(WrapMode::WordChar);

    let cell = CellRendererText::new();
    stc_app.tree_column_name.pack_start(&cell, true);
    stc_app
        .tree_column_name
        .add_attribute(&cell, "text", STC_VIEWER_COLUMN_NAME as i32);
    stc_app.tree_view.append_column(&stc_app.tree_column_name);
    stc_app.tree_view.append_column(&stc_app.tree_column_data);
    stc_app.tree_view.set_headers_visible(false);

    let tree_scroll = ScrolledWindow::new(Adjustment::NONE, Adjustment::NONE);
    tree_scroll.add(&stc_app.tree_view);
    tree_scroll.set_expand(true);

    let left_layout = gtk::Box::new(Orientation::Vertical, 0);
    left_layout.add(&stc_app.search_entry);
    left_layout.add(&tree_scroll);

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
    app_lock
        .compile_button
        .connect_clicked(move |_| app_copy.lock().unwrap().on_cursor_changed());

    window.add(&paned);
    window.show_all();
}
