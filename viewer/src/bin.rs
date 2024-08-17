use stc::prelude::*;

mod app;
mod storage;

#[cfg(feature = "gui-egui")]
mod egui;

#[cfg(feature = "gui-gtk4")]
mod gtk4;

#[cfg(all(feature = "gui-egui", feature = "gui-gtk4"))]
compile_error!(
    "Feature gui-egui and gui-gtk4 are mutually exclusive and cannot be enabled together"
);

#[cfg(feature = "gui-gtk4")]
fn main() {
    gtk4::main()
}

#[cfg(feature = "gui-egui")]
fn main() -> Result<(), eframe::Error> {
    egui::main()
}

pub(crate) trait PrototypeDisplayName {
    fn display_name(&self) -> String;
}

impl PrototypeDisplayName for Prototype {
    fn display_name(&self) -> String {
        let proto = self.read().unwrap();
        let name = proto.name();

        if name.is_empty() {
            format!("{} - {} (No Name)", proto.decl().kind(), proto.id())
        } else {
            name.origin_string().to_owned()
        }
    }
}
