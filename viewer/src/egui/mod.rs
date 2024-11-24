use crate::app::StcViewerApp;
use crate::{PrototypeContent, PrototypeDisplayName};
use eframe::egui;
use eframe::egui::{vec2, CollapsingHeader, FontId, Label, RichText, TextEdit};
use log::*;
use stc::prelude::*;
use std::default::Default;

#[derive(Default)]
struct StcViewerEGui {
    app: StcViewerApp,

    // UI stuffs
    search_text: String,
    content: RichText,
    show_origin_ast: bool,
    current_function: Option<Function>,
}

impl StcViewerEGui {
    pub fn new(app: StcViewerApp) -> Self {
        Self {
            app,

            ..Default::default()
        }
    }

    // UI stuffs
    fn set_content(&mut self, text: impl Into<String>) {
        self.content = RichText::new(text).font(FontId::monospace(12.));
    }

    fn show_func(&mut self) {
        if let Some(f) = self.current_function.clone() {
            let f = f.read();
            let content = match (self.show_origin_ast, f.compiled_code()) {
                (false, Some(code)) => format!("{}", code),
                _ => format!("{}", f.parse_tree()),
            };

            self.set_content(content);
        }
    }

    fn ui_app(&mut self, ui: &mut egui::Ui, app: ModuleContext) {
        let app = app.read();
        CollapsingHeader::new(format!("App {}", app.id()))
            .default_open(true)
            .show(ui, |ui| {
                // Prototypes
                CollapsingHeader::new("Prototypes")
                    .default_open(true)
                    .show(ui, |ui| {
                        for proto in app.declarations() {
                            if ui.button(proto.display_name().to_string()).clicked() {
                                self.set_content(proto.content());
                            }
                        }
                    });

                // Functions
                CollapsingHeader::new("Functions")
                    .default_open(true)
                    .show(ui, |ui| {
                        for func in app.functions() {
                            let proto = app
                                .get_declaration_by_id(func.read().decl_id())
                                .unwrap()
                                .display_name();

                            if ui.button(proto).clicked() {
                                self.current_function = Some(func.clone());
                                self.show_func();
                            }
                        }
                    });
            });
    }
}

impl eframe::App for StcViewerEGui {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Left panel
        egui::SidePanel::left("left")
            .resizable(true)
            .show(ctx, |ui| {
                ui.vertical(|ui| {
                    ui.horizontal(|ui| {
                        // Search input
                        let input = TextEdit::singleline(&mut self.search_text)
                            .min_size(vec2(100., 0.))
                            .desired_width(200.);
                        ui.add(input);
                        // ui.add_sized([100f32, ui.available_height()], input);

                        // Refresh button
                        let search_button = ui.button("Search");
                        if search_button.clicked() {
                            trace!("Search clicked")
                        }
                    });

                    // Data-Tree
                    let contexts: Vec<_> = self
                        .app
                        .mgr
                        .read()
                        .contexts()
                        .filter(|x| x.read().is_app_ctx())
                        .cloned()
                        .collect();
                    for app_ctx in contexts {
                        self.ui_app(ui, app_ctx.clone());
                    }
                });
            });

        // Right panel
        egui::CentralPanel::default().show(ctx, |ui| {
            // Tools Bar
            ui.horizontal(|ui| {
                let compile_button = ui.button("Compile");
                if compile_button.clicked() {
                    self.app.compile();
                    self.show_func();
                }

                let show_ast = ui.checkbox(&mut self.show_origin_ast, "Show AST");
                if show_ast.clicked() {
                    self.show_func();
                }
            });

            // Content Area
            egui::ScrollArea::vertical()
                .auto_shrink(true)
                .show(ui, |ui| {
                    ui.add(Label::new(self.content.clone()).selectable(true));
                });
        });
    }
}

pub fn main() -> Result<(), eframe::Error> {
    pretty_env_logger::init();

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([800.0, 600.0]),
        ..Default::default()
    };

    let viewer = StcViewerEGui::new(StcViewerApp::load_test_project());
    eframe::run_native(
        "stc-viewer - egui",
        options,
        Box::new(|_cc| {
            // for images support
            // egui_extras::install_image_loaders(&cc.egui_ctx);

            Ok(Box::new(viewer))
        }),
    )
}
