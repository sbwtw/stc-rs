use crate::PrototypeDisplayName;
use eframe::egui::{Response, Ui, Widget};
use stc::prelude::*;

pub(crate) struct AppWidget {
    app: ModuleContext,
}

impl AppWidget {
    pub fn new(app: ModuleContext) -> Self {
        Self { app }
    }
}

impl Widget for AppWidget {
    fn ui(self, ui: &mut Ui) -> Response {
        ui.label(format!("App {}", self.app.read().id()));

        // Prototypes
        let r = ui.label("\tPrototypes");
        for proto in self.app.read().declarations() {
            ui.label(format!("\t\t{}", proto.display_name()));
        }

        r
    }
}
