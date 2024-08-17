mod widgets;
use widgets::*;

use crate::app::StcViewerApp;
use crate::storage;
use eframe::egui;
use log::*;
use quick_xml::de::from_str;
use stc::prelude::*;
use std::default::Default;

#[derive(Default)]
struct StcViewerEGui {
    app: StcViewerApp,

    // UI stuffs
    search_text: String,
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
                        ui.text_edit_singleline(&mut self.search_text);

                        // Refresh button
                        let search_button = ui.button("Search");
                        if search_button.clicked() {
                            trace!("Search clicked")
                        }
                    });

                    // Data-Tree
                    if let Some(active_app) = self.app.mgr.read().active_application() {
                        let app_widget = AppWidget::new(active_app);
                        ui.add(app_widget);
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
            });

            // Content Area
            egui::ScrollArea::vertical()
                .auto_shrink(true)
                .show(ui, |ui| {
                    // TODO: multi line content
                    ui.add(egui::Label::new(&self.search_text).selectable(true));
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

    let mgr = UnitsManager::new();
    let proj: Result<storage::Application, _> =
        from_str(include_str!("../../test_projects/example1/test_proj.xml"));
    let ctx: ModuleContext = proj.unwrap().into();
    mgr.write().add_context(ctx.clone());
    mgr.write().set_active_application(Some(ctx.read().id()));

    let viewer = StcViewerEGui::new(StcViewerApp::with_mgr(mgr));
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
