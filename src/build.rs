fn main() {
    #[cfg(feature = "use_lalrpop")]
    lalrpop::Configuration::new()
        .use_cargo_dir_conventions()
        .process_file("src/parser/lalrpop_impl/st.lalrpop")
        .unwrap();
}
