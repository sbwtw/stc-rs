mod storage;

#[cfg(feature = "gui-egui")]
mod egui;

#[cfg(feature = "gui-gtk4")]
mod column_object;
#[cfg(feature = "gui-gtk4")]
mod gtk4;
#[cfg(feature = "gui-gtk4")]
mod stc_viewer;

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
