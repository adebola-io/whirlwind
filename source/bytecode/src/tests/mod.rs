use std::path::PathBuf;

use analyzer::{Module, Standpoint, CORE_LIBRARY_PATH};

#[test]
fn retrieves_main_module() {
    let text = String::from(
        "
    module Main;

    public function main() {
        
    }
    ",
    );
    let mut module = Module::from_text(&text);
    module.module_path = Some(PathBuf::from("testing://Main.wrl"));
    let corelib_path = PathBuf::from(CORE_LIBRARY_PATH);
    let mut standpoint = Standpoint::new(true, Some(corelib_path));
    let idx = standpoint.add_module(module).unwrap();
    standpoint.validate();
    standpoint.entry_module = idx;
    assert!(standpoint.diagnostics.is_empty());

    println!("The main function is {:#?}", standpoint.main());
}
