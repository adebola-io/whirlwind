use crate::{
    predefined::PAD,
    vm::{Constant, SizeRegistry, VM},
};
use bytecode::Opcode;

fn run<const T: usize>(instructions: [u8; T]) {
    print_instructions(&instructions);
    let mut vm = VM::new(SizeRegistry::empty(), vec![]);
    vm.run(&instructions);
}

fn print_instructions(instructions: &[u8]) {
    print!("Running ");
    for byte in instructions {
        print!("{:02X} ", byte);
    }
    print!("\n\n");
}

#[test]
fn test_runtime_exit() {
    run([PAD, Opcode::Exit.into()]);
}

#[test]
fn test_runtime_hello_world() {
    let index = 0usize.to_be_bytes();
    let instructions = [
        PAD,
        Opcode::LoadIconstptra.into(),
        index[0],
        index[1],
        index[2],
        index[3],
        index[4],
        index[5],
        index[6],
        index[7],
        Opcode::Printconstptra.into(),
        0,
        // Print "Hello, world." and exit.
    ];
    print_instructions(&instructions);
    let mut vm = VM::new(
        SizeRegistry::empty(),
        vec![Constant::String(String::from("Hello, world.\n"))],
    );
    vm.run(&instructions);
}

#[test]
fn test_runtime_add_numbers() {
    // Print 4 + 5;
    run([
        PAD,
        Opcode::LoadIacc8.into(),
        4,
        Opcode::LoadIr8.into(),
        5,
        Opcode::Addacc.into(),
        0,
        0,
        Opcode::Printacc8.into(),
        0,
    ]);
}

// #[test]
// fn test_square_root_of_number() {
//     run([
//         PAD,
//         Opcode::LoadIacc8.into(),
//         4,
//         Opcode::Sqrtacc.into(),
//         0,
//         0,
//         Opcode::Printacc8.into(),
//         0,
//     ]); // Print sqrt(4);
// }
