use std::path::PathBuf;

use analyzer::{Module, Standpoint, CORE_LIBRARY_PATH};

use crate::{from_text::bytecode_from_text, CallablePtr, Opcode};

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

// #[test]
fn simple_empty_main() {
    let object = bytecode_from_text(
        "
    module main;

    function main() {

    }
    ",
    )
    .unwrap();
    assert_eq!(
        object.functions,
        vec![CallablePtr {
            name: String::from("main"),
            param_count: 0,
            start: 7, // After pad + call + index + exit.
            calls: 0
        }]
    );
    assert_eq!(
        object.instructions,
        vec![
            Opcode::LoadFunctionPtr.into(),
            0,
            0,
            0,
            0, // address of main.
        ]
    )
}

// #[test]
fn function_call() {
    let object = bytecode_from_text(
        "
    module main;

    function main() {
        anotherFunction()
    }
    function anotherFunction() {

    }
    ",
    )
    .unwrap();
    assert_eq!(
        object.functions,
        vec![
            CallablePtr {
                name: String::from("main"),
                param_count: 0,
                start: 7, // After pad + call + index + exit.
                calls: 0
            },
            CallablePtr {
                name: String::from("anotherFunction"),
                param_count: 0,
                start: 15,
                calls: 0
            }
        ]
    )
}

// #[test]
fn recursive_function_call() {
    let object = bytecode_from_text(
        "
    module main;

    function main() {
        anotherFunction()
    }
    function anotherFunction() {
        main();
        anotherFunction();
    }
    ",
    )
    .unwrap();
    assert_eq!(
        object.functions,
        vec![
            CallablePtr {
                name: String::from("main"),
                param_count: 0,
                start: 7, // After pad + call + index + exit.
                calls: 0
            },
            CallablePtr {
                name: String::from("anotherFunction"),
                param_count: 0,
                start: 15,
                calls: 0
            }
        ]
    )
}
