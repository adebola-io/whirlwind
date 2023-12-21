use crate::{
    predefined::PAD,
    vm::{Function, SizeRegistry, VM},
};
use bytecode::Opcode;

fn run(instructions: &[u8]) {
    print_instructions(&instructions);
    let mut vm = VM::new();
    vm.instructions = instructions.to_vec();
    vm.define_main_function(Function::main());
    vm.run().unwrap();
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
    run(&[PAD, Opcode::Exit.into()]);
}

#[test]
fn test_runtime_hello_world() {
    // Print "Hello, world." and exit.
    let index = 0usize.to_be_bytes();
    let instructions = vec![
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
        Opcode::Exit.into(),
    ];
    print_instructions(&instructions);
    let mut vm = VM::new();
    vm.define_main_function(Function::main());
    vm.instructions = instructions;
    vm.constants.add(String::from("Hello, world.\n"));
    vm.run().unwrap();
}

#[test]
fn test_runtime_add_numbers() {
    // Print 4 + 5;
    run(&[
        PAD,
        Opcode::LoadIacc8.into(),
        4,
        Opcode::LoadIr8.into(),
        5,
        Opcode::Addacc.into(),
        0,
        0,
        Opcode::Printacc8.into(),
        Opcode::Exit.into(),
    ]);
}

#[test]
fn test_runtime_square_root_of_number() {
    // Print sqrt(4);
    run(&[
        PAD,
        Opcode::LoadIacc8.into(),
        4,
        Opcode::Sqrtacc.into(),
        0,
        0,
        Opcode::Printacc64.into(),
        Opcode::Exit.into(),
    ]);
}

#[test]
fn test_runtime_function_call_and_return() {
    let mut vm = VM::new();
    vm.define_main_function(Function::main());
    vm.functions.push(Function {
        name: "AnotherFunction".to_owned(),
        start: 7,
        frame_size: 8,
        calls: 0,
    });
    let function_idx = 1u32.to_be_bytes();
    let constidx = vm
        .constants
        .add(String::from("Hello from inside a function()!\n"))
        .to_be_bytes();
    vm.instructions = vec![
        PAD,
        // Main:
        Opcode::Call.into(),
        function_idx[0],
        function_idx[1],
        function_idx[2],
        function_idx[3],
        Opcode::Exit.into(),
        // AnotherFunction:
        Opcode::LoadIconstptra.into(),
        constidx[0],
        constidx[1],
        constidx[2],
        constidx[3],
        constidx[4],
        constidx[5],
        constidx[6],
        constidx[7],
        Opcode::Printconstptra.into(),
        Opcode::Return.into(),
    ];
    print_instructions(&vm.instructions);
    vm.run().unwrap();
}

#[test]
fn test_runtime_loops() {
    let mut start = 26usize.to_be_bytes();
    let end = 26usize.to_be_bytes();
    let count = 100_000_000usize.to_be_bytes();

    let mut instructions = vec![PAD, Opcode::LoopFor.into()];
    instructions.append(&mut start.to_vec());
    instructions.append(&mut end.to_vec());
    instructions.append(&mut count.to_vec());
    instructions.append(&mut vec![Opcode::Stall.into(), Opcode::Exit.into()]);

    run(instructions.as_slice());
}

#[test]
fn test_runtime_if_else() {
    // if 10 + 20 == 30 {
    //  exit
    // }
    // print 30
    let jumpdest = 23usize.to_be_bytes();
    let code = vec![
        PAD,
        Opcode::LoadIacc8.into(),
        10,
        Opcode::LoadIr8.into(),
        20,
        Opcode::Addacc.into(),
        0,
        0,
        Opcode::LoadIr8.into(),
        30,
        Opcode::Eqacc.into(),
        0,
        0,
        Opcode::JumpIfTrue.into(),
        jumpdest[0],
        jumpdest[1],
        jumpdest[2],
        jumpdest[3],
        jumpdest[4],
        jumpdest[5],
        jumpdest[6],
        jumpdest[7],
        Opcode::Printacc8.into(),
        Opcode::Exit.into(),
    ];
    run(&code);
}

#[test]
#[should_panic]
fn test_stack_overflow() {
    // Call Main() recursively.
    let func_idx = 0usize.to_be_bytes();
    run(&[
        PAD,
        Opcode::Call.into(),
        func_idx[0],
        func_idx[1],
        func_idx[2],
        func_idx[3],
        Opcode::Exit.into(),
    ])
}

#[test]
fn test_runtime_variable_init() {
    let zeroaddr = 0u32.to_be_bytes();
    run(&[
        PAD,
        Opcode::LoadIframe.into(),
        zeroaddr[0],
        zeroaddr[1],
        zeroaddr[2],
        zeroaddr[3],
        0,
        3,
        Opcode::Printframe.into(),
        zeroaddr[0],
        zeroaddr[1],
        zeroaddr[2],
        zeroaddr[3],
        0,
        Opcode::Exit.into(),
    ]);
}
