use std::env;

fn main() {
    #[cfg(feature = "use_lalrpop")]
    lalrpop::Configuration::new()
        .use_cargo_dir_conventions()
        .process_file("src/parser/lalrpop_impl/st.lalrpop")
        .unwrap();

    let manifest = env::var("CARGO_MANIFEST_DIR").unwrap();
    cbindgen::Builder::new()
        .with_crate(manifest)
        .generate()
        .unwrap()
        .write_to_file("scanner.h");
}
