#![cfg(test)]

use whirl_ast::{
    AccessExpr, ArrayExpr, AssignmentExpr, BinaryExpr, Block, CallExpr, DiscreteType, Else,
    EnumDeclaration, Expression, FunctionDeclaration, FunctionExpr, Identifier, IfExpression,
    IndexExpr, LogicExpr, Parameter, ScopeAddress, ScopeEntry, Span, Statement, TestDeclaration,
    Type, TypeDeclaration, TypeExpression, UnaryExpr, UseDeclaration, UsePath, UseTarget,
    WhirlNumber, WhirlString,
};

use crate::parse_text;

#[test]
fn parsing_public_functions() {
    let mut parser = parse_text("public function CalculateCost(name) {}");

    let statement = parser.next().unwrap().unwrap();

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0].into(),
            body: Block::empty([1, 37, 1, 39].into()),
            span: [1, 1, 1, 39].into()
        })
    )
}

#[test]
fn parsing_functions_with_types() {
    // Normal types.
    let mut parser = parse_text("function GenerateHash(name: String, id: Integer) {}");
    let statement = parser.next().unwrap().unwrap();

    assert!(parser
        .scope_manager()
        .lookaround("GenerateHash")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f) if matches!
            (
                &f.params[0].type_label.declared,
                Some(TypeExpression::Discrete(d)) if d.name.name == "String"
            ) && matches!
            (
                &f.params[1].type_label.declared,
                Some(TypeExpression::Discrete(d)) if d.name.name == "Integer"
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0].into(),
            body: Block::empty([1, 50, 1, 52].into()),
            span: [1, 1, 1, 52].into()
        })
    )
}

#[test]
fn parsing_functions_with_member_types() {
    let mut parser = parse_text("function WriteFile(path: Core.Io.Path) {}");
    let statement = parser.next().unwrap().unwrap();

    assert!(parser
        .scope_manager()
        .lookaround("WriteFile")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f) if matches!
            (
                &f.params[0].type_label.declared,
                Some(TypeExpression::Member(m)) if matches!(
                    &*m.namespace, TypeExpression::Member(_)
                )
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0].into(),
            body: Block::empty([1, 40, 1, 42].into()),
            span: [1, 1, 1, 42].into()
        })
    )
}

#[test]
fn parsing_functions_with_generic_types() {
    let mut parser = parse_text("function Combine(title: ArrayOf<String>) {}");
    let mut statement = parser.next().unwrap().unwrap();

    assert!(parser
        .scope_manager()
        .lookaround("Combine")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f) if matches!
            (
                &f.params[0].type_label.declared,
                Some(TypeExpression::Discrete(d)) if
                    d.name.name == "ArrayOf" &&
                    d.generic_args.as_ref().unwrap().len() == 1
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0].into(),
            body: Block::empty([1, 42, 1, 44].into()),
            span: [1, 1, 1, 44].into()
        })
    );

    // Nested.
    parser = parse_text("function Flatten(nested: ArrayOf<ArrayOf<ArrayOf<String>>>) {}");
    statement = parser.next().unwrap().unwrap();

    assert!(parser
        .scope_manager()
        .lookaround("Flatten")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f)
            if matches!
            (
                &f.params[0].type_label.declared,
                Some(TypeExpression::Discrete(d)) if
                    d.name.name == "ArrayOf" &&
                    d.generic_args.as_ref().unwrap().len() == 1
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0].into(),
            body: Block::empty([1, 61, 1, 63].into()),
            span: [1, 1, 1, 63].into()
        })
    )
}

#[test]
fn parsing_functional_types() {
    let mut parser = parse_text("function Find(predicate: fn(value: Name): Boolean) {}");
    let statement = parser.next().unwrap().unwrap();

    // println!(
    //     "{:?}",
    //     parser.scope_manager().lookaround("Find").unwrap().entry
    // );

    assert!(parser
        .scope_manager()
        .lookaround("Find")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f) if matches!
            (
                &f.params[0].type_label.declared,
                Some(TypeExpression::Functional(f)) if matches!
                {
                    f.params[0].type_label.declared,
                    Some(TypeExpression::Discrete(_))
                } && f.return_type.is_some()
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0].into(),
            body: Block::empty([1, 52, 1, 54].into()),
            span: [1, 1, 1, 54].into()
        })
    );
}

#[test]
fn parsing_union_types() {
    let mut parser = parse_text(
        "
        function EatHealthy(
            fruit: Banana 
                   | Pawpaw 
                   | Mango 
                   | Secret.HiddenFruit
                   | ArrayOf<Fruit>
                   | fn(): Fruit
            ) 
        {}",
    );
    let statement = parser.next().unwrap().unwrap();

    // println!(
    //     "{:#?}",
    //     parser
    //         .scope_manager()
    //         .lookaround("EatHealthy")
    //         .unwrap()
    //         .entry
    // );

    assert!(parser
        .scope_manager()
        .lookaround("EatHealthy")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f) if matches!
            (
                &f.params[0].type_label.declared,
                Some(TypeExpression::Union(u)) if u.types.len() == 6
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0].into(),
            body: Block::empty([10, 9, 10, 11].into()),
            span: [2, 9, 10, 11].into()
        })
    );
}

#[test]
fn parsing_functions() {
    // Empty function.
    let mut parser = parse_text("function SayHello(){}");

    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: ScopeAddress::from([0, 0]),
            body: Block::empty(Span::from([1, 20, 1, 22])),
            span: Span::from([1, 1, 1, 22])
        })
    );

    // Nested function.
    parser = parse_text(
        "
    function SayHello() {
        function SayHelloAgain() {
            // Code.
        }
    }
    ",
    );

    let mut statement = parser.next().unwrap().unwrap();
    let mut scope_manager = parser.scope_manager();

    assert!(scope_manager.is_global());
    assert_eq!(scope_manager.len(), 3);
    assert!(scope_manager.lookdown("SayHelloAgain").is_some());

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: ScopeAddress::from([0, 0]),
            body: Block {
                statements: vec![Statement::FunctionDeclaration(FunctionDeclaration {
                    address: ScopeAddress::from([1, 0]),
                    body: Block::empty(Span::from([3, 34, 5, 10])),
                    span: Span::from([3, 9, 5, 10])
                })],
                span: Span::from([2, 25, 6, 6])
            },
            span: Span::from([2, 5, 6, 6])
        })
    );

    // With parameters.
    parser = parse_text("function GreetUser(name, id?){}");

    statement = parser.next().unwrap().unwrap();
    scope_manager = parser.scope_manager();

    assert!(scope_manager.lookaround("GreetUser").is_some());

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: ScopeAddress::from([0, 0]),
            body: Block {
                statements: vec![],
                span: Span::from([1, 30, 1, 32])
            },
            span: Span::from([1, 1, 1, 32])
        })
    );
}

#[test]
fn parsing_async_functions() {
    // Async function
    let mut parser = parse_text("async function GreetUser(name, id?){}");

    let statement = parser.next().unwrap().unwrap();
    let scope_manager = parser.scope_manager();

    assert!(scope_manager.lookaround("GreetUser").is_some_and(
        |search| matches!(search.entry, whirl_ast::ScopeEntry::Function(f) if f.is_async)
    ));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: ScopeAddress::from([0, 0]),
            body: Block {
                statements: vec![],
                span: Span::from([1, 36, 1, 38])
            },
            span: Span::from([1, 1, 1, 38])
        })
    );
}

#[test]
fn parsing_type_declarations() {
    let mut parser = parse_text("public type Number = SignedInteger | UnsignedInteger | Float;");

    let statement = parser.next().unwrap().unwrap();
    let scope_manager = parser.scope_manager();

    assert!(scope_manager.lookaround("Number").is_some_and(
        |search| matches!(search.entry, whirl_ast::ScopeEntry::Type(t) if t.is_public)
    ));

    assert_eq!(
        statement,
        Statement::TypeDeclaration(TypeDeclaration {
            address: ScopeAddress::from([0, 0]),
            span: Span::from([1, 1, 1, 60])
        })
    );
}

#[test]
fn parsing_multiple_statements() {
    let mut parser = parse_text(
        "
        /// Returns an iterable parser for text input.
public function parse_text(input: String | Path, parserOptions?: ParserOptions): Parser<TextLexer> {
    // Parser::from_lexer(lex_text(input))
}

/// Animals are multicellular eukaryotic organisms in the biological kingdom Animalia.
public type Animal = Cat | Dog | Sheep | Goat;

/// Any statement representation.
type Statement =
  | ExpressionStatement
  | IfStatement
  | WhileStatement
  | DoWhileStatement
  | SwitchStatement
  | EmptyStatement
  | ForStatement
  | ForInStatement
  | BlockStatement
  | ReturnStatement
  | TryStatement
  | LabeledStatement
  | ThrowStatement;

type Predicate = fn(value?: ArrayOf<String>): Boolean;
    ",
    );

    let mut statements = vec![];
    for statement in &mut parser {
        statements.push(statement);
    }
    let scope_manager = parser.scope_manager();

    assert!(scope_manager
        .lookaround("Statement")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Type(t) if matches!(
                &t.value,
                TypeExpression::Union(u) if u.types.len() == 13
            )
        )));
    assert_eq!(statements.len(), 4)
}

#[test]
fn parsing_this_type() {
    let mut parser = parse_text("type Self = This;");

    let statement = parser.next().unwrap().unwrap();
    let scope_manager = parser.scope_manager();

    assert!(scope_manager
        .lookaround("Self")
        .is_some_and(|search| matches!(
            search.entry, whirl_ast::ScopeEntry::Type(t) if matches!(
                t.value,
                TypeExpression::This { .. }
            )
        )));

    assert_eq!(
        statement,
        Statement::TypeDeclaration(TypeDeclaration {
            address: ScopeAddress::from([0, 0]),
            span: Span::from([1, 1, 1, 16])
        })
    );
}

#[test]
fn test_testing_block() {
    let mut parser = parse_text("test 'test stuff' {}");

    let statement = parser.next().unwrap().unwrap();

    assert_eq!(
        statement,
        Statement::TestDeclaration(TestDeclaration {
            name: format!("test stuff"),
            name_span: [1, 6, 1, 18].into(),
            body: Block::empty([1, 19, 1, 21].into()),
            span: [1, 1, 1, 21].into()
        })
    );
}

#[test]
fn parsing_enum_variant() {
    // Normal Enums.
    let mut parser = parse_text(
        "
     enum Scope {
        Private,
        Public
    }",
    );

    let mut statement = parser.next().unwrap().unwrap();

    let mut scope_manager = parser.scope_manager();

    assert!(scope_manager
        .lookaround("Scope")
        .is_some_and(|search| matches!(
            search.entry, whirl_ast::ScopeEntry::Enum(e) if matches!(
                e.variants[1].name.name.as_str(),
                "Public"
            )
        )));

    assert_eq!(
        statement,
        Statement::EnumDeclaration(EnumDeclaration {
            address: [0, 0].into(),
            span: Span::from([2, 6, 5, 6])
        })
    );

    // Tagged Enums.
    parser = parse_text(
        "
     enum Node {
        Root,
        Child(Node)
    }",
    );

    statement = parser.next().unwrap().unwrap();

    scope_manager = parser.scope_manager();

    assert!(scope_manager
        .lookaround("Node")
        .is_some_and(|search| matches!(
            search.entry, whirl_ast::ScopeEntry::Enum(e) if matches!(
                e.variants[1].name.name.as_str(),
                "Child"
            ) && e.variants[1].tagged_type.is_some()
        )));

    assert_eq!(
        statement,
        Statement::EnumDeclaration(EnumDeclaration {
            address: [0, 0].into(),
            span: Span::from([2, 6, 5, 6])
        })
    );
}

#[test]
fn parses_use_import() {
    // Simple.
    let mut parser = parse_text("use OtherModule;");

    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::UseDeclaration(UseDeclaration {
            target: UseTarget {
                name: Identifier {
                    name: format!("OtherModule"),
                    span: [1, 5, 1, 15].into()
                },
                path: UsePath::Me
            },
            is_public: false,
            span: [1, 1, 1, 17].into()
        })
    );

    // Public.
    parser = parse_text("public use OtherModule;");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::UseDeclaration(UseDeclaration {
            target: UseTarget {
                name: Identifier {
                    name: format!("OtherModule"),
                    span: [1, 12, 1, 22].into()
                },
                path: UsePath::Me
            },
            is_public: true,
            span: [1, 1, 1, 24].into()
        })
    );

    // One item.
    parser = parse_text("use Core.Math;");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::UseDeclaration(UseDeclaration {
            target: UseTarget {
                name: Identifier {
                    name: format!("Core"),
                    span: [1, 5, 1, 8].into()
                },
                path: UsePath::Item(Box::new(UseTarget {
                    name: Identifier {
                        name: format!("Math"),
                        span: [1, 10, 1, 13].into()
                    },
                    path: UsePath::Me
                }))
            },
            is_public: false,
            span: [1, 1, 1, 15].into()
        })
    );
}

#[test]
fn parse_nested_use_item() {
    let mut parser = parse_text("use Components.UI.Buttons.ErrorButton;");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::UseDeclaration(UseDeclaration {
            target: UseTarget {
                name: Identifier {
                    name: format!("Components"),
                    span: [1, 5, 1, 14].into()
                },
                path: UsePath::Item(Box::new(UseTarget {
                    name: Identifier {
                        name: format!("UI"),
                        span: [1, 16, 1, 17].into()
                    },
                    path: UsePath::Item(Box::new(UseTarget {
                        name: Identifier {
                            name: format!("Buttons"),
                            span: [1, 19, 1, 25].into()
                        },
                        path: UsePath::Item(Box::new(UseTarget {
                            name: Identifier {
                                name: format!("ErrorButton"),
                                span: [1, 27, 1, 37].into()
                            },
                            path: UsePath::Me
                        }))
                    }))
                }))
            },
            is_public: false,
            span: [1, 1, 1, 39].into()
        })
    )
}

#[test]
fn parse_group_use_import() {
    // Nested One item.
    let mut parser = parse_text("use Components.UI.{Button, Alert};");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::UseDeclaration(UseDeclaration {
            target: UseTarget {
                name: Identifier {
                    name: format!("Components"),
                    span: [1, 5, 1, 14].into()
                },
                path: UsePath::Item(Box::new(UseTarget {
                    name: Identifier {
                        name: format!("UI"),
                        span: [1, 16, 1, 17].into()
                    },
                    path: UsePath::List(vec![
                        UseTarget {
                            name: Identifier {
                                name: format!("Button"),
                                span: [1, 20, 1, 25].into()
                            },
                            path: UsePath::Me
                        },
                        UseTarget {
                            name: Identifier {
                                name: format!("Alert"),
                                span: [1, 28, 1, 32].into()
                            },
                            path: UsePath::Me
                        }
                    ])
                }))
            },
            is_public: false,
            span: [1, 1, 1, 35].into()
        })
    )
}

#[test]
fn parse_call_expressions() {
    // One argument
    let mut parser = parse_text("Calculate(A)");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::CallExpr(Box::new(CallExpr {
            caller: Expression::Identifier(Identifier {
                name: format!("Calculate"),
                span: [1, 1, 1, 9].into()
            }),
            arguments: vec![Expression::Identifier(Identifier {
                name: format!("A"),
                span: [1, 11, 1, 11].into()
            })],
            span: [1, 1, 1, 13].into()
        })))
    );

    // Many arguments
    parser = parse_text("Calculate(A, B, C, D)");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::CallExpr(Box::new(CallExpr {
            caller: Expression::Identifier(Identifier {
                name: format!("Calculate"),
                span: [1, 1, 1, 9].into()
            }),
            arguments: vec![
                Expression::Identifier(Identifier {
                    name: format!("A"),
                    span: [1, 11, 1, 11].into()
                }),
                Expression::Identifier(Identifier {
                    name: format!("B"),
                    span: [1, 14, 1, 14].into()
                }),
                Expression::Identifier(Identifier {
                    name: format!("C"),
                    span: [1, 17, 1, 17].into()
                }),
                Expression::Identifier(Identifier {
                    name: format!("D"),
                    span: [1, 20, 1, 20].into()
                })
            ],
            span: [1, 1, 1, 22].into()
        })))
    );

    // Nested
    parser = parse_text("Calculate(Calculate(Calculate(A)))");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::CallExpr(Box::new(CallExpr {
            caller: Expression::Identifier(Identifier {
                name: format!("Calculate"),
                span: [1, 1, 1, 9].into()
            }),
            arguments: vec![Expression::CallExpr(Box::new(CallExpr {
                caller: Expression::Identifier(Identifier {
                    name: format!("Calculate"),
                    span: [1, 11, 1, 19].into()
                }),
                arguments: vec![Expression::CallExpr(Box::new(CallExpr {
                    caller: Expression::Identifier(Identifier {
                        name: format!("Calculate"),
                        span: [1, 21, 1, 29].into()
                    }),
                    arguments: vec![Expression::Identifier(Identifier {
                        name: format!("A"),
                        span: [1, 31, 1, 31].into()
                    }),],
                    span: [1, 21, 1, 33].into()
                }))],
                span: [1, 11, 1, 34].into()
            }))],
            span: [1, 1, 1, 35].into()
        })))
    );
}

#[test]
fn parse_string_literal() {
    let mut parser = parse_text("\"Hello, world!\"");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::StringLiteral(whirl_ast::WhirlString {
            value: format!("Hello, world!"),
            span: [1, 1, 1, 16].into()
        }))
    );
}

#[test]
fn parse_fn_expressions() {
    let mut parser = parse_text("fn (a: Number): Number a");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::FnExpr(Box::new(FunctionExpr {
            generic_params: None,
            params: vec![Parameter {
                name: Identifier {
                    name: format!("a"),
                    span: [1, 5, 1, 5].into()
                },
                type_label: Type {
                    declared: Some(TypeExpression::Discrete(DiscreteType {
                        name: Identifier {
                            name: format!("Number"),
                            span: [1, 8, 1, 13].into()
                        },
                        generic_args: None,
                        span: [1, 8, 1, 13].into()
                    })),
                    inferred: None
                },
                is_optional: false
            }],
            return_type: Type {
                declared: Some(TypeExpression::Discrete(DiscreteType {
                    name: Identifier {
                        name: format!("Number"),
                        span: [1, 17, 1, 22].into()
                    },
                    generic_args: None,
                    span: [1, 17, 1, 22].into()
                })),
                inferred: None
            },
            body: Expression::Identifier(Identifier {
                name: format!("a"),
                span: [1, 24, 1, 24].into()
            }),
            span: [1, 1, 1, 24].into()
        })))
    );
}

#[test]
fn parse_if_expressions() {
    // Without else.
    let mut parser = parse_text("if IsLegal() { \"Come on in\" }");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::IfExpr(Box::new(IfExpression {
            condition: Expression::CallExpr(Box::new(CallExpr {
                caller: Expression::Identifier(Identifier {
                    name: format!("IsLegal"),
                    span: [1, 4, 1, 10].into()
                }),
                arguments: vec![],
                span: [1, 4, 1, 13].into()
            })),
            consequent: Block {
                statements: vec![Statement::FreeExpression(Expression::StringLiteral(
                    WhirlString {
                        value: format!("Come on in"),
                        span: [1, 16, 1, 28].into()
                    }
                ))],
                span: [1, 14, 1, 30].into()
            },
            alternate: None,
            span: [1, 1, 1, 30].into()
        })))
    );

    // With else.
    parser = parse_text("if IsLegal() { \"Come on in\" } else { \"You are not eligible.\" }");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::IfExpr(Box::new(IfExpression {
            condition: Expression::CallExpr(Box::new(CallExpr {
                caller: Expression::Identifier(Identifier {
                    name: format!("IsLegal"),
                    span: [1, 4, 1, 10].into()
                }),
                arguments: vec![],
                span: [1, 4, 1, 13].into()
            })),
            consequent: Block {
                statements: vec![Statement::FreeExpression(Expression::StringLiteral(
                    WhirlString {
                        value: format!("Come on in"),
                        span: [1, 16, 1, 28].into()
                    }
                ))],
                span: [1, 14, 1, 30].into()
            },
            alternate: Some(Else {
                expression: Expression::BlockExpr(Block {
                    statements: vec![Statement::FreeExpression(Expression::StringLiteral(
                        WhirlString {
                            value: format!("You are not eligible."),
                            span: [1, 38, 1, 61].into()
                        }
                    ))],
                    span: [1, 36, 1, 63].into()
                }),
                span: [1, 31, 1, 63].into()
            }),
            span: [1, 1, 1, 63].into()
        })))
    );
}

#[test]
fn parse_shorthand_variables() {
    // Simple.
    let mut parser = parse_text("message := GetMessage();");

    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::ShorthandVariableDeclaration(whirl_ast::ShorthandVariableDeclaration {
            address: [0, 0].into(),
            value: Expression::CallExpr(Box::new(CallExpr {
                caller: Expression::Identifier(Identifier {
                    name: format!("GetMessage"),
                    span: [1, 12, 1, 21].into()
                }),
                arguments: vec![],
                span: [1, 12, 1, 24].into()
            })),
            span: [1, 1, 1, 25].into()
        })
    );

    let scope_manager = parser.scope_manager();

    assert!(matches!(
        scope_manager.lookaround("message").unwrap().entry,
        ScopeEntry::Variable(whirl_ast::VariableSignature {name,..}) if name.name == format!("message")));

    // With type
    parser = parse_text("array: ArrayOf<String> := MakeArray();");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::ShorthandVariableDeclaration(whirl_ast::ShorthandVariableDeclaration {
            address: [0, 0].into(),
            value: Expression::CallExpr(Box::new(CallExpr {
                caller: Expression::Identifier(Identifier {
                    name: format!("MakeArray"),
                    span: [1, 27, 1, 35].into()
                }),
                arguments: vec![],
                span: [1, 27, 1, 38].into()
            })),
            span: [1, 1, 1, 39].into()
        })
    );
}

#[test]
fn parse_arrays() {
    let mut parser = parse_text("[a, b, c, d]");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::ArrayExpr(ArrayExpr {
            elements: vec![
                Expression::Identifier(Identifier {
                    name: format!("a"),
                    span: [1, 2, 1, 2].into()
                }),
                Expression::Identifier(Identifier {
                    name: format!("b"),
                    span: [1, 5, 1, 5].into()
                }),
                Expression::Identifier(Identifier {
                    name: format!("c"),
                    span: [1, 8, 1, 8].into()
                }),
                Expression::Identifier(Identifier {
                    name: format!("d"),
                    span: [1, 11, 1, 11].into()
                })
            ],
            span: [1, 1, 1, 13].into()
        }))
    )
}

#[test]
fn parse_index_expression() {
    let mut parser = parse_text("a[b]");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::IndexExpr(Box::new(IndexExpr {
            object: Expression::Identifier(Identifier {
                name: format!("a"),
                span: [1, 1, 1, 1].into()
            }),
            index: Expression::Identifier(Identifier {
                name: format!("b"),
                span: [1, 3, 1, 3].into()
            }),
            span: [1, 1, 1, 5].into()
        })))
    )
}

#[test]
fn parse_binary_expression() {
    // Same operator.
    let mut parser = parse_text("2 + 2 + 2");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::BinaryExpr(Box::new(BinaryExpr {
            left: Expression::BinaryExpr(Box::new(BinaryExpr {
                left: Expression::NumberLiteral(WhirlNumber {
                    value: whirl_ast::Number::Decimal(format!("2")),
                    span: [1, 1, 1, 1].into()
                }),
                operator: whirl_ast::BinOperator::Add,
                right: Expression::NumberLiteral(WhirlNumber {
                    value: whirl_ast::Number::Decimal(format!("2")),
                    span: [1, 5, 1, 5].into()
                }),
                span: [1, 1, 1, 5].into()
            })),
            operator: whirl_ast::BinOperator::Add,
            right: Expression::NumberLiteral(WhirlNumber {
                value: whirl_ast::Number::Decimal(format!("2")),
                span: [1, 9, 1, 9].into()
            }),
            span: [1, 1, 1, 9].into()
        })))
    );

    // Different operators.
    parser = parse_text("2 + 3 * 4");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::BinaryExpr(Box::new(BinaryExpr {
            left: Expression::NumberLiteral(WhirlNumber {
                value: whirl_ast::Number::Decimal(format!("2")),
                span: [1, 1, 1, 1].into()
            }),
            operator: whirl_ast::BinOperator::Add,
            right: Expression::BinaryExpr(Box::new(BinaryExpr {
                left: Expression::NumberLiteral(WhirlNumber {
                    value: whirl_ast::Number::Decimal(format!("3")),
                    span: [1, 5, 1, 5].into()
                }),
                operator: whirl_ast::BinOperator::Multiply,
                right: Expression::NumberLiteral(WhirlNumber {
                    value: whirl_ast::Number::Decimal(format!("4")),
                    span: [1, 9, 1, 9].into()
                }),
                span: [1, 5, 1, 9].into()
            })),
            span: [1, 1, 1, 9].into()
        })))
    );

    // Raised to operator.
    parser = parse_text("2 ^ 3 ^ 4");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::BinaryExpr(Box::new(BinaryExpr {
            left: Expression::NumberLiteral(WhirlNumber {
                value: whirl_ast::Number::Decimal(format!("2")),
                span: [1, 1, 1, 1].into()
            }),
            operator: whirl_ast::BinOperator::PowerOf,
            right: Expression::BinaryExpr(Box::new(BinaryExpr {
                left: Expression::NumberLiteral(WhirlNumber {
                    value: whirl_ast::Number::Decimal(format!("3")),
                    span: [1, 5, 1, 5].into()
                }),
                operator: whirl_ast::BinOperator::PowerOf,
                right: Expression::NumberLiteral(WhirlNumber {
                    value: whirl_ast::Number::Decimal(format!("4")),
                    span: [1, 9, 1, 9].into()
                }),
                span: [1, 5, 1, 9].into()
            })),
            span: [1, 1, 1, 9].into()
        })))
    )
}

#[test]
fn parse_logical_expression() {
    // Logical
    let mut parser = parse_text("isTrue || isFalse");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::LogicExpr(Box::new(LogicExpr {
            left: Expression::Identifier(Identifier {
                name: format!("isTrue"),
                span: [1, 1, 1, 6].into()
            }),
            operator: whirl_ast::LogicOperator::Or,
            right: Expression::Identifier(Identifier {
                name: format!("isFalse"),
                span: [1, 11, 1, 17].into()
            }),
            span: [1, 1, 1, 17].into()
        })))
    );

    // literal and multiple.
    parser = parse_text("isTrue || isFalse and isTrue");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::LogicExpr(Box::new(LogicExpr {
            left: Expression::LogicExpr(Box::new(LogicExpr {
                left: Expression::Identifier(Identifier {
                    name: format!("isTrue"),
                    span: [1, 1, 1, 6].into()
                }),
                operator: whirl_ast::LogicOperator::Or,
                right: Expression::Identifier(Identifier {
                    name: format!("isFalse"),
                    span: [1, 11, 1, 17].into()
                }),
                span: [1, 1, 1, 17].into()
            })),
            operator: whirl_ast::LogicOperator::AndLiteral,
            right: Expression::Identifier(Identifier {
                name: format!("isTrue"),
                span: [1, 23, 1, 28].into()
            }),
            span: [1, 1, 1, 28].into()
        })))
    );
}

#[test]
fn parse_dot_expression() {
    // Simple
    let mut parser = parse_text("Core.Fmt");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::AccessExpr(Box::new(AccessExpr {
            object: Expression::Identifier(Identifier {
                name: format!("Core"),
                span: [1, 1, 1, 4].into()
            }),
            property: Expression::Identifier(Identifier {
                name: format!("Fmt"),
                span: [1, 6, 1, 8].into()
            }),
            span: [1, 1, 1, 8].into()
        })))
    );

    // nested
    parser = parse_text("Core.Fmt.Println()");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::CallExpr(Box::new(CallExpr {
            caller: Expression::AccessExpr(Box::new(AccessExpr {
                object: Expression::AccessExpr(Box::new(AccessExpr {
                    object: Expression::Identifier(Identifier {
                        name: format!("Core"),
                        span: [1, 1, 1, 4].into()
                    }),
                    property: Expression::Identifier(Identifier {
                        name: format!("Fmt"),
                        span: [1, 6, 1, 8].into()
                    }),
                    span: [1, 1, 1, 8].into()
                })),
                property: Expression::Identifier(Identifier {
                    name: format!("Println"),
                    span: [1, 10, 1, 16].into()
                }),
                span: [1, 1, 1, 16].into()
            })),
            arguments: vec![],
            span: [1, 1, 1, 19].into()
        })))
    );
}

#[test]
fn parse_assignment_expression() {
    // simple.
    let mut parser = parse_text("a = b");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::AssignmentExpr(Box::new(AssignmentExpr {
            left: Expression::Identifier(Identifier {
                name: format!("a"),
                span: [1, 1, 1, 1].into()
            }),
            operator: whirl_ast::AssignOperator::Assign,
            right: Expression::Identifier(Identifier {
                name: format!("b"),
                span: [1, 5, 1, 5].into()
            }),
            span: [1, 1, 1, 5].into()
        })))
    );

    // nested.
    let mut parser = parse_text("a = b = c");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::AssignmentExpr(Box::new(AssignmentExpr {
            left: Expression::Identifier(Identifier {
                name: format!("a"),
                span: [1, 1, 1, 1].into()
            }),
            operator: whirl_ast::AssignOperator::Assign,
            right: Expression::AssignmentExpr(Box::new(AssignmentExpr {
                left: Expression::Identifier(Identifier {
                    name: format!("b"),
                    span: [1, 5, 1, 5].into()
                }),
                operator: whirl_ast::AssignOperator::Assign,
                right: Expression::Identifier(Identifier {
                    name: format!("c"),
                    span: [1, 9, 1, 9].into()
                }),
                span: [1, 5, 1, 9].into()
            })),
            span: [1, 1, 1, 9].into()
        })))
    );
}

#[test]
fn parse_unary_expression() {
    let mut parser = parse_text("!a");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::UnaryExpr(Box::new(UnaryExpr {
            operator: whirl_ast::UnaryOperator::Negation,
            operand: Expression::Identifier(Identifier {
                name: format!("a"),
                span: [1, 2, 1, 2].into()
            }),
            span: [1, 1, 1, 2].into()
        })))
    );
}
