use crate::app::StcViewerApp;
use crate::{PrototypeContent, PrototypeDisplayName};
use eframe::egui;
use eframe::egui::{vec2, CollapsingHeader, FontId, Label, RichText, TextEdit};
use log::*;
use std::default::Default;

#[derive(Default)]
struct StcViewerEGui {
    app: StcViewerApp,

    // UI stuffs
    search_text: String,
    content: RichText,
    show_origin_ast: bool,
}

impl StcViewerEGui {
    pub fn new(app: StcViewerApp) -> Self {
        Self {
            app,

            ..Default::default()
        }
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
                    if let Some(active_app) = self.app.mgr.read().active_application() {
                        let active_app = active_app.read();
                        CollapsingHeader::new(format!("App {}", active_app.id()))
                            .default_open(true)
                            .show(ui, |ui| {
                                // Prototypes
                                CollapsingHeader::new("Prototypes").default_open(true).show(
                                    ui,
                                    |ui| {
                                        for proto in active_app.declarations() {
                                            if ui.button(proto.display_name().to_string()).clicked()
                                            {
                                                self.content = RichText::new(proto.content())
                                                    .font(FontId::monospace(12.));
                                            }
                                        }
                                    },
                                );

                                // Functions
                                CollapsingHeader::new("Functions").default_open(true).show(
                                    ui,
                                    |ui| {
                                        for func in active_app.functions() {
                                            let func = func.read();
                                            let proto = active_app
                                                .get_declaration_by_id(func.decl_id())
                                                .unwrap();

                                            if ui.button(proto.display_name().to_string()).clicked()
                                            {
                                                let content = match (
                                                    self.show_origin_ast,
                                                    func.compiled_code(),
                                                ) {
                                                    (false, Some(code)) => format!("{}", code),
                                                    _ => format!("{}", func.parse_tree()),
                                                };
                                                self.content = RichText::new(content)
                                                    .font(FontId::monospace(12.));
                                            }
                                        }
                                    },
                                );
                            });
                    }
                });
            });

        // Right panel
        egui::CentralPanel::default().show(ctx, |ui| {
            // Tools Bar
            ui.horizontal(|ui| {
                let compile_button = ui.button("Compile");
                if compile_button.clicked() {
                    self.app.compile()
                }

                ui.checkbox(&mut self.show_origin_ast, "Show AST");
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
