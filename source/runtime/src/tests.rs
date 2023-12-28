use crate::vm::VM;
use bytecode::{print_instructions, FunctionPtr, Layout, Opcode, PAD};

fn run(instructions: &[u8]) {
    print_instructions(&instructions);
    let mut vm = VM::new();
    vm.instructions = instructions.to_vec();
    vm.define_main_function(FunctionPtr::main());
    vm.run().unwrap();
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
    vm.define_main_function(FunctionPtr::main());
    vm.instructions = instructions;
    vm.constant_pool.add(String::from("Hello, world.\n"));
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
    let four_64_bit = 25f64.to_be_bytes();
    run(&[
        PAD,
        Opcode::LoadIacc64.into(),
        four_64_bit[0],
        four_64_bit[1],
        four_64_bit[2],
        four_64_bit[3],
        four_64_bit[4],
        four_64_bit[5],
        four_64_bit[6],
        four_64_bit[7],
        Opcode::Sqrtacc64.into(),
        0,
        Opcode::Printacc64.into(),
        Opcode::Exit.into(),
    ]);
}

#[test]
fn test_runtime_function_call_and_return() {
    let mut vm = VM::new();
    vm.define_main_function(FunctionPtr::main());
    vm.dispatch_table.push(FunctionPtr {
        name: String::from("AnotherFunction"),
        start: 11,
        calls: 0,
    });
    let function_idx = 1usize.to_be_bytes();
    let constidx = vm
        .constant_pool
        .add(String::from("Hello from inside a function!\n"))
        .to_be_bytes();
    vm.instructions = vec![
        PAD,
        // Main:
        Opcode::CallNamedFunction.into(),
        function_idx[0],
        function_idx[1],
        function_idx[2],
        function_idx[3],
        function_idx[4],
        function_idx[5],
        function_idx[6],
        function_idx[7],
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
    println!("Size: {}bytes", vm.instructions.len());
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
#[should_panic = "called `Result::unwrap()` on an `Err` value: StackOverflow"]
fn test_stack_overflow() {
    // Call main() recursively.
    let func_idx = 0usize.to_be_bytes();
    run(&[
        PAD,
        Opcode::CallNamedFunction.into(),
        func_idx[0],
        func_idx[1],
        func_idx[2],
        func_idx[3],
        func_idx[4],
        func_idx[5],
        func_idx[6],
        func_idx[7],
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
        Opcode::Exit.into(),
    ]);
}

#[test]
fn create_instance_on_heap() {
    let mut vm = VM::new();
    vm.define_main_function(FunctionPtr::main());
    // model Person {
    //   var id: UInt8;
    //   new() {
    //     this.id = 0;
    //   }
    // }
    // new Person();
    vm.layouts.push(Layout {
        size: 24 * 1,
        property_offsets: vec![0],
    });
    let layoutidx = 0usize.to_be_bytes();
    let frameaddr = 0u32.to_be_bytes();
    let instructions = vec![
        PAD,
        Opcode::StartBlock.into(),
        Opcode::NewInstanceValueA.into(),
        layoutidx[0],
        layoutidx[1],
        layoutidx[2],
        layoutidx[3],
        layoutidx[4],
        layoutidx[5],
        layoutidx[6],
        layoutidx[7],
        Opcode::StoreValueAToFrame.into(),
        frameaddr[0],
        frameaddr[1],
        frameaddr[2],
        frameaddr[3],
        Opcode::Printframe.into(),
        frameaddr[0],
        frameaddr[1],
        frameaddr[2],
        frameaddr[3],
        Opcode::EndBlock.into(),
        Opcode::Exit.into(),
    ];
    print_instructions(&instructions);
    println!("{vm:?}");
    vm.instructions = instructions;
    vm.run().unwrap();
}
