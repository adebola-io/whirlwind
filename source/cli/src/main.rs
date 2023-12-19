mod terminal;

use vm::{Constant, SizeRegistry, VM};

fn main() {
    let v = 0usize.to_be_bytes();
    let instructions = [
        6, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], 7,
        0, // Print "Hello, world." and exit.
    ];
    let mut vm = VM::new(
        SizeRegistry {},
        vec![
            Constant::String(String::from("Hello, world.")),
            Constant::String(String::from("\n")),
        ],
    );
    unsafe { vm.run(&instructions) };
}
