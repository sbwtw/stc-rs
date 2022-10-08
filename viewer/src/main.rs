mod stc_viewer;

use crate::stc_viewer::StcViewerApp;

use gtk::prelude::*;
use gtk::{
    Adjustment, Application, ApplicationWindow, CellRendererText, Orientation, Paned,
    ScrolledWindow, WindowPosition, WrapMode,
};
use stc::ast::Statement;
use stc::context::{ModuleContext, ModuleContextScope, Scope, UnitsManager};
use stc::parser::{StDeclarationParser, StFunctionParser, StLexer};
use stc::transform::TypeAnalyzer;
use stc::utils;

use std::fs::OpenOptions;
use std::process::Command;

fn display_ast(statement: &Statement) {
    // graphviz
    // dump dot file
    {
        let mut out = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open("test.dot")
            .unwrap();

        let mut graphviz = utils::GraphvizExporter::new(&mut out);
        graphviz.plot_statement(statement);
    }

    // convert to svg
    {
        Command::new("dot")
            .args(&["-Tsvg", "test.dot", "-o", "test.svg"])
            .status()
            .expect("failed.");
    }
}

fn main() {
    let mgr = UnitsManager::new();
    let mgr_ui_app = mgr.clone();
    let app = ModuleContext::new(ModuleContextScope::Application);
    let app_id = app.read().id();
    mgr.write().add_context(app);

    let app = mgr.write().get_context(app_id).unwrap();
    let decl = StLexer::new("function test: int VAR a: INT; b: INT; END_VAR end_function");
    let decl = StDeclarationParser::new().parse(decl).unwrap();
    let decl_id = app.write().add_declaration(decl);

    let prg = StLexer::new("program prg: int VAR a: BYTE; END_VAR end_program");
    let prg = StDeclarationParser::new().parse(prg).unwrap();
    let _prg_id = app.write().add_declaration(prg);

    let global = StLexer::new("VAR_GLOBAL END_VAR VAR_GLOBAL 全局变量1: REAL; END_VAR");
    let global = StDeclarationParser::new().parse(global).unwrap();
    let _global_id = app.write().add_declaration(global);

    let body = StLexer::new("if a < 全局变量1 then prg.a := 1; else b := 2; end_if");
    let body = StFunctionParser::new().parse(body).unwrap();
    app.write().add_function(decl_id, body);

    let app = app.read();
    let fun = app.get_function(decl_id);

    let mut type_analyzer = TypeAnalyzer::new();

    // analysis declarations
    // for decl in app.get_declaration_by_id()

    // analysis function
    if let Some(f) = fun {
        let mut f = f.write().unwrap();

        let scope = Scope::new(Some(mgr), Some(app_id), Some(decl_id));
        type_analyzer.analyze_statement(f.body_mut(), scope);

        display_ast(f.body());

        println!("{}", f.body());
    }

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
    stc_app.tree_column_name.add_attribute(&cell, "text", 0);
    stc_app.tree_view.append_column(&stc_app.tree_column_name);
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

    let right_layout = gtk::Box::new(Orientation::Vertical, 0);
    right_layout.add(&button_layout);
    right_layout.add(&content_scroll);

    let paned = Paned::new(Orientation::Horizontal);
    paned.add(&left_layout);
    paned.add(&right_layout);

    let app = stc_app.clone();
    stc_app.refresh_button.connect_clicked(move |_| {
        app.refresh();
    });

    // stc_app
    //     .tree_view
    //     .connect_row_expanded(move |view, _, _| view.queue_compute_expand());

    window.add(&paned);
    window.show_all();
}
