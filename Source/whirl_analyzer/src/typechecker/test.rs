#![cfg(test)]

use crate::{analyze_text, TypeError};

#[test]
fn test_adding_string_and_number() {
    let mut typechecker = analyze_text(
        "
    function Main() {
        a := 1 + true;
    }
    ",
    );

    assert_eq!(
        typechecker.next().unwrap(),
        vec![TypeError {
            _type: crate::TypeErrorType::InvalidBinary {
                left: whirl_ast::TypeEval::Pointer {
                    address: [0, 1].into(),
                    args: None
                },
                operator: whirl_ast::BinOperator::Add,
                right: whirl_ast::TypeEval::Pointer {
                    address: [0, 2].into(),
                    args: None
                }
            },
            spans: vec![[3, 14, 3, 22].into()]
        }]
    )
}
