#![cfg(test)]

use whirl_errors::TypeErrorType;

use crate::{analyze_text, TypeError};

#[test]
fn test_adding_string_and_number() {
    let mut inferrer = analyze_text(
        "
    function Main() {
        a := 1 + true;
    }
    ",
    );

    inferrer.infer();

    assert_eq!(
        inferrer.type_errors.get_mut()[0],
        TypeError {
            _type: crate::TypeErrorType::InvalidBinary {
                left: whirl_ast::TypeEval::Instance {
                    address: [0, 1].into(),
                    args: None
                },
                operator: whirl_ast::BinOperator::Add,
                right: whirl_ast::TypeEval::Instance {
                    address: [0, 2].into(),
                    args: None
                }
            },
            span: [3, 14, 3, 22].into()
        }
    )
}

#[test]
fn test_global_control_statements() {
    let mut inferrer = analyze_text(
        "if 1 + 1 == 2 {
            // do stuff.
        }",
    );

    inferrer.infer();

    assert_eq!(
        inferrer.type_errors.get_mut()[0],
        TypeError {
            _type: TypeErrorType::GlobalControl,
            span: [1, 1, 3, 10].into()
        }
    )
}

#[test]
fn test_parameter_types() {
    let mut inferrer = analyze_text(
        "function PrintName(name): String {
        }",
    );

    inferrer.infer();

    assert_eq!(
        inferrer.type_errors.get_mut()[0],
        TypeError {
            _type: TypeErrorType::UninferrableParameter(format!("name")),
            span: [1, 20, 1, 24].into()
        }
    )
}
