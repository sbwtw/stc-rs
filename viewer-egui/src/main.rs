use eframe::egui;
use stc::prelude::*;
use std::default::Default;

#[derive(Default)]
struct StcViewer {
    _mgr: UnitsManager,

    name: String,
}

impl eframe::App for StcViewer {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::SidePanel::left("left")
            .resizable(true)
            .show(ctx, |ui| {
                ui.vertical(|ui| {
                    ui.horizontal(|ui| {
                        // Search input
                        ui.text_edit_singleline(&mut self.name);

                        // Refresh button
                        let _refresh_button = ui.button("Refresh");
                    });

                    // Data-Tree
                });
            });

        egui::CentralPanel::default().show(ctx, |ui| {
            // Tools Bar
            ui.horizontal(|ui| {
                _ = ui.button("Reload");
                _ = ui.button("Compile");
            });

            // Content Area
            egui::ScrollArea::vertical()
                .auto_shrink(true)
                .show(ui, |ui| {
                    ui.add(egui::Label::new(&self.name).selectable(true));
                });
        });
    }
}

fn main() -> Result<(), eframe::Error> {
    pretty_env_logger::init();

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([800.0, 600.0]),
        ..Default::default()
    };

    eframe::run_native(
        "stc-viewer - egui",
        options,
        Box::new(|_cc| {
            // for images support
            // egui_extras::install_image_loaders(&cc.egui_ctx);

            Ok(Box::<StcViewer>::default())
        }),
    )
}
