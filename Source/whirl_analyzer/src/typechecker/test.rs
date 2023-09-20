#![cfg(test)]

use whirl_errors::TypeErrorType;

use crate::{analyze_text, TypeError};

#[test]
fn test_adding_string_and_number() {
    let mut infer = analyze_text(
        "
    function Main() {
        a := 1 + true;
    }
    ",
    );

    assert_eq!(
        infer.next().unwrap(),
        vec![TypeError {
            _type: crate::TypeErrorType::InvalidBinary {
                left: whirl_ast::TypeEval::TypeWithinModule {
                    address: [0, 1].into(),
                    args: None
                },
                operator: whirl_ast::BinOperator::Add,
                right: whirl_ast::TypeEval::TypeWithinModule {
                    address: [0, 2].into(),
                    args: None
                }
            },
            span: [3, 14, 3, 22].into()
        }]
    )
}

#[test]
fn test_global_control_statements() {
    let mut infer = analyze_text(
        "if 1 + 1 == 2 {
            // do stuff.
        }",
    );

    assert_eq!(
        infer.next().unwrap(),
        vec![TypeError {
            _type: TypeErrorType::GlobalControl,
            span: [1, 1, 3, 10].into()
        }]
    )
}
