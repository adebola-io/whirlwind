#![cfg(test)]

use crate::parse_text;

#[test]
fn parsing_functions() {
    let mut parser = parse_text("function SayHello(){}");

    println!("{:?}", parser.next());
}
