use std::path::PathBuf;

// todo: use virtual fs.
use crate::{Module, PathIndex, Standpoint, CORE_LIBRARY_PATH};

#[test]
fn bind_variables_and_constants() {
    let text = "
            module Test; 

            public function main() {
                greeting := \"Say Hello\";
                const CONSTANT: Number = 9090;
            }
        ";
    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, false).unwrap();
    assert!(standpoint.diagnostics.len() == 1);
    assert!(standpoint
        .symbol_library
        .find(|symbol| symbol.name == "greeting")
        .is_some());
    assert!(standpoint
        .symbol_library
        .find(|symbol| symbol.name == "CONSTANT")
        .is_some());
    println!(
        "{:#?}",
        standpoint
            .symbol_library
            .in_module(PathIndex(0))
            .map(|symbol| (&symbol.name, &symbol.kind, &symbol.references))
            .collect::<Vec<_>>()
    );
    println!(
        "ERRORS: \n\n\n{:#?}",
        standpoint
            .diagnostics
            .iter()
            .filter(|error| error.offending_file == PathIndex(0))
            .collect::<Vec<_>>()
    );
}

#[test]
fn bind_call_expression() {
    let text = "
            module Test;

            public function main() {
                greeting := \"Say Hello\";
                println(greeting);
            }
        ";
    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, false).unwrap();
    assert!(standpoint.diagnostics.len() == 1);
    assert!(standpoint
        .symbol_library
        .find(|symbol| symbol.name == "println")
        .is_some());
    assert!(standpoint
        .symbol_library
        .find(|symbol| symbol.name == "greeting")
        .is_some());
}

#[test]
fn bind_models() {
    let text = "
            module Test;

            public model Car {
                var make: String,
                var year: UnsignedInt,
                public function honk() {

                }
                public function [Vehicle.start]<U>() {

                }
            }
        ";
    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, false).unwrap();
    assert!(standpoint
        .symbol_library
        .find(|symbol| symbol.name == "Car")
        .is_some());
    assert!(standpoint
        .symbol_library
        .find(|symbol| symbol.name == "honk")
        .is_some());
    println!("{:#?}", standpoint.symbol_library);
}

#[test]
fn bind_interfaces() {
    let text = "
            module Test;

            public interface Vehicle {
                public function start(): This;
            }
        ";
    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, false).unwrap();
    assert!(standpoint
        .symbol_library
        .find(|symbol| symbol.name == "Vehicle")
        .is_some());
    assert!(standpoint
        .symbol_library
        .find(|symbol| symbol.name == "start")
        .is_some());
    println!("{:#?}", standpoint.symbol_library);
}

#[test]
fn bind_this() {
    let text = "
            module Test;

            public model Unit {
                public function clone(): This {
                    return this;
                }
            }
        ";
    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, false).unwrap();
    assert!(standpoint.diagnostics.len() == 0);
}

#[test]
fn test_enum_type() {
    let text = "
            module Test;

            public enum Color {
                Red,
                Orange(Color),
                Green
            }
        ";
    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, false).unwrap();
    println!(
        "{:#?}",
        standpoint
            .symbol_library
            .in_module(PathIndex(0))
            .map(|symbol| (&symbol.name, &symbol.kind, &symbol.references))
            .collect::<Vec<_>>()
    );
    println!(
        "ERRORS: \n\n\n{:#?}",
        standpoint
            .diagnostics
            .iter()
            .filter(|error| error.offending_file == PathIndex(0))
            .collect::<Vec<_>>()
    );
    assert!(standpoint.diagnostics.len() == 0);
}

#[test]
fn test_fn_expr() {
    let text = "
            module Test;

            function main() {
                square := fn(a) a * 2;
            }
        ";
    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, false).unwrap();
    println!(
        "{:#?}",
        standpoint
            .symbol_library
            .in_module(PathIndex(0))
            .map(|symbol| (&symbol.name, &symbol.kind, &symbol.references))
            .collect::<Vec<_>>()
    );
    println!(
        "ERRORS: \n\n\n{:#?}",
        standpoint
            .diagnostics
            .iter()
            .filter(|error| error.offending_file == PathIndex(0))
            .collect::<Vec<_>>()
    );
    assert!(standpoint.diagnostics.len() == 0);
}

#[test]
fn test_use_import() {
    let text = "
            module Test;

            use core.io.println;

            function main() {
                println(\"Hello, world!\");
            }
        ";
    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, false).unwrap();
    assert!(standpoint
        .symbol_library
        .find(|symbol| symbol.name == "println")
        .is_some());
    assert!(standpoint
        .symbol_library
        .find(|symbol| symbol.name == "main")
        .is_some());
    assert!(standpoint.diagnostics.len() == 0);
    println!(
        "{:#?}",
        standpoint
            .symbol_library
            .in_module(PathIndex(0))
            .map(|symbol| (&symbol.name, &symbol.kind, &symbol.references))
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_function() {
    let text = "
        module Main;

function main() {
}

/// Adds two numbers together.
function Add(a: Int, b: Int): Int {
    return a + b;
}

        ";

    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, true).unwrap();
    println!(
        "{:#?}",
        standpoint
            .symbol_library
            .in_module(PathIndex(0))
            .collect::<Vec<_>>()
    );
}

#[test]
fn show_imports() {
    let text = "
        module Test;

        use utils.sum;

        public function main() {
            return Sum(1, 2);
        }
        ";

    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, true).unwrap();
    println!(
        "{:#?}",
        standpoint
            .symbol_library
            .in_module(PathIndex(0))
            .collect::<Vec<_>>()
    );
    println!("{:#?}", standpoint.diagnostics);
}

#[test]
fn resolve_single_module_imports() {
    let module0_text = "
    module Main;

    use Test.utils.add;

    
        ";
    let module1_text = "
    module Utils;

    type Int = Int;

    public function add(a: Int, b: Int): Int {
        return a + b;
    }
    ";

    let module2_text = "
    module Test;

    public use Utils;
    public use Utils.Add;

    public function main() {
        return Add(1, 2);
    }
    ";

    let mut main_module = Module::from_text(module0_text);
    main_module.module_path = Some(PathBuf::from("testing://Main.wrl"));
    let mut utils_module = Module::from_text(module1_text);
    utils_module.module_path = Some(PathBuf::from("testing://Utils.wrl"));
    let mut test_module = Module::from_text(module2_text);
    test_module.module_path = Some(PathBuf::from("testing://Test.wrl"));

    let mut standpoint = Standpoint::build_from_module(utils_module, false).unwrap();
    standpoint.auto_update = true;
    standpoint.add_module(test_module);
    standpoint.add_module(main_module);

    println!(
        "{:#?}",
        standpoint
            .symbol_library
            .symbols()
            .map(|(_, symbol)| symbol)
            .collect::<Vec<_>>()
    );
    println!("{:#?}", standpoint.diagnostics);
    // assert!(standpoint.errors.len() == 0);
}

#[test]
fn resolve_mutliple_module_imports() {
    let module0_text = "
    module Main;

    use Test.{Utils.Add, Divide};

    
        ";
    let module1_text = "
    module Utils;

    type Int = Int;

    public function Add(a: Int, b: Int): Int {
        return a + b;
    }

    public function Divide(a: Int, b: Int): Int {
        return a / b;
    }
    ";

    let module2_text = "
    module Test;

    public use Utils;
    public use Utils.Add;
    public use Utils.Divide;

    public type Function = fn (a: Function): Function;

    public function main() {
        return Add(1, 2);
    }
    ";

    let mut main_module = Module::from_text(module0_text);
    main_module.module_path = Some(PathBuf::from("testing://Main.wrl"));
    let mut utils_module = Module::from_text(module1_text);
    utils_module.module_path = Some(PathBuf::from("testing://Utils.wrl"));
    let mut test_module = Module::from_text(module2_text);
    test_module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let mut standpoint = Standpoint::build_from_module(utils_module, false).unwrap();
    standpoint.auto_update = true;
    standpoint.add_module(test_module);
    standpoint.add_module(main_module);

    println!(
        "{:?}",
        standpoint
            .symbol_library
            .symbols()
            .map(|(_, symbol)| (&symbol.name, &symbol.references, &symbol.kind))
            .collect::<Vec<_>>()
    );
    println!("{:#?}", standpoint.diagnostics);
    // assert!(standpoint.errors.len() == 0);
}

#[test]
fn test_duplication() {
    let text = "
            module Test;

            public type Maybe = Maybe;

            public model Maybe<T> implements Assertable<T> + Try<T> {}

        ";
    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, false).unwrap();
    println!("{:#?}", standpoint);
}

#[test]
fn test_variable_binding() {
    let text = "
            module Test;

            type String = String;

            public function main() {
                var { x, y }: Position;
                var name: String = \"Sefunmi\";
                var [arrayItem1, arrayItem2] = new Array().FillWith(0, 3);
            }
        ";
    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, false).unwrap();
    println!("{:#?}", standpoint.symbol_library);
}

#[test]
fn test_forloop_binding() {
    let text = "
            module Test;

            public function main() {
                a := [1, 2, 3];
                for { num as n } in a {
                    
                }
            }
        ";
    let mut module = Module::from_text(text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let standpoint = Standpoint::build_from_module(module, false).unwrap();
    println!("{:#?}", standpoint.symbol_library);
}

#[test]
fn testing_the_standard_library() {
    let corelib_path = Some(PathBuf::from(CORE_LIBRARY_PATH));
    let mut standpoint = Standpoint::new(true, corelib_path);
    let time = std::time::Instant::now();
    standpoint.validate();
    println!("Built Core in {:?}", time.elapsed());
    for error in standpoint.diagnostics {
        let start = error.span().start;
        let offending_module = standpoint
            .module_map
            .get(error.offending_file)
            .and_then(|module| module.path_buf.to_str());
        println!(
            "In Module:\n {:?}:{:?}:{:?} == {:#?}\n\n",
            offending_module.unwrap(),
            start[0],
            start[1],
            error._type
        );
    }
}

#[test]
fn refreshing() {
    let mut text = String::from(
        "
    module Test;

    public function main() {
        
    }
    ",
    );
    let mut module = Module::from_text(&text);
    module.module_path = Some(PathBuf::from("testing://Test.wrl"));
    let mut standpoint = Standpoint::new(true, Some(PathBuf::from(CORE_LIBRARY_PATH)));
    let idx = standpoint.add_module(module).unwrap();
    standpoint.validate();
    for _ in 0..300 {
        text.push_str("a");
        let time = std::time::Instant::now();
        standpoint.refresh_module(idx, &text);
        println!("{:?}", time.elapsed())
    }
}
