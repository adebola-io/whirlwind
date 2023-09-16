#![cfg(test)]

use whirl_ast::{
    AccessExpr, ArrayExpr, AssignmentExpr, BinaryExpr, Block, CallExpr, DiscreteType, Else,
    EnumDeclaration, Expression, FunctionDeclaration, FunctionExpr, Identifier, IfExpression,
    IndexExpr, LogicExpr, ModelBody, ModelDeclaration, ModelProperty, ModelPropertyType, NewExpr,
    Parameter, ScopeAddress, ScopeEntry, Span, Statement, TestDeclaration, ThisExpr, TraitBody,
    TraitDeclaration, TraitProperty, Type, TypeDeclaration, TypeExpression, UnaryExpr,
    UseDeclaration, UsePath, UseTarget, WhileStatement, WhirlBoolean, WhirlNumber, WhirlString,
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
            span: Span::from([1, 1, 1, 61])
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
            span: Span::from([1, 1, 1, 17])
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
                    span: [1, 5, 1, 16].into()
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
                    span: [1, 12, 1, 23].into()
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
                    span: [1, 5, 1, 9].into()
                },
                path: UsePath::Item(Box::new(UseTarget {
                    name: Identifier {
                        name: format!("Math"),
                        span: [1, 10, 1, 14].into()
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
                    span: [1, 5, 1, 15].into()
                },
                path: UsePath::Item(Box::new(UseTarget {
                    name: Identifier {
                        name: format!("UI"),
                        span: [1, 16, 1, 18].into()
                    },
                    path: UsePath::Item(Box::new(UseTarget {
                        name: Identifier {
                            name: format!("Buttons"),
                            span: [1, 19, 1, 26].into()
                        },
                        path: UsePath::Item(Box::new(UseTarget {
                            name: Identifier {
                                name: format!("ErrorButton"),
                                span: [1, 27, 1, 38].into()
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
                    span: [1, 5, 1, 15].into()
                },
                path: UsePath::Item(Box::new(UseTarget {
                    name: Identifier {
                        name: format!("UI"),
                        span: [1, 16, 1, 18].into()
                    },
                    path: UsePath::List(vec![
                        UseTarget {
                            name: Identifier {
                                name: format!("Button"),
                                span: [1, 20, 1, 26].into()
                            },
                            path: UsePath::Me
                        },
                        UseTarget {
                            name: Identifier {
                                name: format!("Alert"),
                                span: [1, 28, 1, 33].into()
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
                span: [1, 1, 1, 10].into()
            }),
            arguments: vec![Expression::Identifier(Identifier {
                name: format!("A"),
                span: [1, 11, 1, 12].into()
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
                span: [1, 1, 1, 10].into()
            }),
            arguments: vec![
                Expression::Identifier(Identifier {
                    name: format!("A"),
                    span: [1, 11, 1, 12].into()
                }),
                Expression::Identifier(Identifier {
                    name: format!("B"),
                    span: [1, 14, 1, 15].into()
                }),
                Expression::Identifier(Identifier {
                    name: format!("C"),
                    span: [1, 17, 1, 18].into()
                }),
                Expression::Identifier(Identifier {
                    name: format!("D"),
                    span: [1, 20, 1, 21].into()
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
                span: [1, 1, 1, 10].into()
            }),
            arguments: vec![Expression::CallExpr(Box::new(CallExpr {
                caller: Expression::Identifier(Identifier {
                    name: format!("Calculate"),
                    span: [1, 11, 1, 20].into()
                }),
                arguments: vec![Expression::CallExpr(Box::new(CallExpr {
                    caller: Expression::Identifier(Identifier {
                        name: format!("Calculate"),
                        span: [1, 21, 1, 30].into()
                    }),
                    arguments: vec![Expression::Identifier(Identifier {
                        name: format!("A"),
                        span: [1, 31, 1, 32].into()
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
                    span: [1, 5, 1, 6].into()
                },
                info: None,
                type_label: Type {
                    declared: Some(TypeExpression::Discrete(DiscreteType {
                        name: Identifier {
                            name: format!("Number"),
                            span: [1, 8, 1, 14].into()
                        },
                        generic_args: None,
                        span: [1, 8, 1, 14].into()
                    })),
                    inferred: None
                },
                is_optional: false
            }],
            return_type: Type {
                declared: Some(TypeExpression::Discrete(DiscreteType {
                    name: Identifier {
                        name: format!("Number"),
                        span: [1, 17, 1, 23].into()
                    },
                    generic_args: None,
                    span: [1, 17, 1, 23].into()
                })),
                inferred: None
            },
            body: Expression::Identifier(Identifier {
                name: format!("a"),
                span: [1, 24, 1, 25].into()
            }),
            span: [1, 1, 1, 25].into()
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
                    span: [1, 4, 1, 11].into()
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
                    span: [1, 4, 1, 11].into()
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
                    span: [1, 12, 1, 22].into()
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
                    span: [1, 27, 1, 36].into()
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
                    span: [1, 2, 1, 3].into()
                }),
                Expression::Identifier(Identifier {
                    name: format!("b"),
                    span: [1, 5, 1, 6].into()
                }),
                Expression::Identifier(Identifier {
                    name: format!("c"),
                    span: [1, 8, 1, 9].into()
                }),
                Expression::Identifier(Identifier {
                    name: format!("d"),
                    span: [1, 11, 1, 12].into()
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
                span: [1, 1, 1, 2].into()
            }),
            index: Expression::Identifier(Identifier {
                name: format!("b"),
                span: [1, 3, 1, 4].into()
            }),
            span: [1, 1, 1, 5].into()
        })))
    )
}

#[test]
fn parse_new_expression() {
    let mut parser = parse_text("new Stack()");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::NewExpr(Box::new(NewExpr {
            value: Expression::CallExpr(Box::new(CallExpr {
                caller: Expression::Identifier(Identifier {
                    name: format! {"Stack"},
                    span: [1, 5, 1, 10].into()
                }),
                arguments: vec![],
                span: [1, 5, 1, 12].into()
            })),
            span: [1, 1, 1, 12].into(),
        })))
    )
}

#[test]
fn parse_this_expression() {
    let mut parser = parse_text("this");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::ThisExpr(ThisExpr {
            span: [1, 1, 1, 5].into()
        }))
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
                    span: [1, 1, 1, 2].into()
                }),
                operator: whirl_ast::BinOperator::Add,
                right: Expression::NumberLiteral(WhirlNumber {
                    value: whirl_ast::Number::Decimal(format!("2")),
                    span: [1, 5, 1, 6].into()
                }),
                span: [1, 1, 1, 6].into()
            })),
            operator: whirl_ast::BinOperator::Add,
            right: Expression::NumberLiteral(WhirlNumber {
                value: whirl_ast::Number::Decimal(format!("2")),
                span: [1, 9, 1, 10].into()
            }),
            span: [1, 1, 1, 10].into()
        })))
    );

    // Different operators.
    parser = parse_text("2 + 3 * 4");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::BinaryExpr(Box::new(BinaryExpr {
            left: Expression::NumberLiteral(WhirlNumber {
                value: whirl_ast::Number::Decimal(format!("2")),
                span: [1, 1, 1, 2].into()
            }),
            operator: whirl_ast::BinOperator::Add,
            right: Expression::BinaryExpr(Box::new(BinaryExpr {
                left: Expression::NumberLiteral(WhirlNumber {
                    value: whirl_ast::Number::Decimal(format!("3")),
                    span: [1, 5, 1, 6].into()
                }),
                operator: whirl_ast::BinOperator::Multiply,
                right: Expression::NumberLiteral(WhirlNumber {
                    value: whirl_ast::Number::Decimal(format!("4")),
                    span: [1, 9, 1, 10].into()
                }),
                span: [1, 5, 1, 10].into()
            })),
            span: [1, 1, 1, 10].into()
        })))
    );

    // Raised to operator.
    parser = parse_text("2 ^ 3 ^ 4");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::BinaryExpr(Box::new(BinaryExpr {
            left: Expression::NumberLiteral(WhirlNumber {
                value: whirl_ast::Number::Decimal(format!("2")),
                span: [1, 1, 1, 2].into()
            }),
            operator: whirl_ast::BinOperator::PowerOf,
            right: Expression::BinaryExpr(Box::new(BinaryExpr {
                left: Expression::NumberLiteral(WhirlNumber {
                    value: whirl_ast::Number::Decimal(format!("3")),
                    span: [1, 5, 1, 6].into()
                }),
                operator: whirl_ast::BinOperator::PowerOf,
                right: Expression::NumberLiteral(WhirlNumber {
                    value: whirl_ast::Number::Decimal(format!("4")),
                    span: [1, 9, 1, 10].into()
                }),
                span: [1, 5, 1, 10].into()
            })),
            span: [1, 1, 1, 10].into()
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
                span: [1, 1, 1, 7].into()
            }),
            operator: whirl_ast::LogicOperator::Or,
            right: Expression::Identifier(Identifier {
                name: format!("isFalse"),
                span: [1, 11, 1, 18].into()
            }),
            span: [1, 1, 1, 18].into()
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
                    span: [1, 1, 1, 7].into()
                }),
                operator: whirl_ast::LogicOperator::Or,
                right: Expression::Identifier(Identifier {
                    name: format!("isFalse"),
                    span: [1, 11, 1, 18].into()
                }),
                span: [1, 1, 1, 18].into()
            })),
            operator: whirl_ast::LogicOperator::AndLiteral,
            right: Expression::Identifier(Identifier {
                name: format!("isTrue"),
                span: [1, 23, 1, 29].into()
            }),
            span: [1, 1, 1, 29].into()
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
                span: [1, 1, 1, 5].into()
            }),
            property: Expression::Identifier(Identifier {
                name: format!("Fmt"),
                span: [1, 6, 1, 9].into()
            }),
            span: [1, 1, 1, 9].into()
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
                        span: [1, 1, 1, 5].into()
                    }),
                    property: Expression::Identifier(Identifier {
                        name: format!("Fmt"),
                        span: [1, 6, 1, 9].into()
                    }),
                    span: [1, 1, 1, 9].into()
                })),
                property: Expression::Identifier(Identifier {
                    name: format!("Println"),
                    span: [1, 10, 1, 17].into()
                }),
                span: [1, 1, 1, 17].into()
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
                span: [1, 1, 1, 2].into()
            }),
            operator: whirl_ast::AssignOperator::Assign,
            right: Expression::Identifier(Identifier {
                name: format!("b"),
                span: [1, 5, 1, 6].into()
            }),
            span: [1, 1, 1, 6].into()
        })))
    );

    // nested.
    let mut parser = parse_text("a = b = c");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::AssignmentExpr(Box::new(AssignmentExpr {
            left: Expression::Identifier(Identifier {
                name: format!("a"),
                span: [1, 1, 1, 2].into()
            }),
            operator: whirl_ast::AssignOperator::Assign,
            right: Expression::AssignmentExpr(Box::new(AssignmentExpr {
                left: Expression::Identifier(Identifier {
                    name: format!("b"),
                    span: [1, 5, 1, 6].into()
                }),
                operator: whirl_ast::AssignOperator::Assign,
                right: Expression::Identifier(Identifier {
                    name: format!("c"),
                    span: [1, 9, 1, 10].into()
                }),
                span: [1, 5, 1, 10].into()
            })),
            span: [1, 1, 1, 10].into()
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
                span: [1, 2, 1, 3].into()
            }),
            span: [1, 1, 1, 3].into()
        })))
    );
}

#[test]
fn parse_complex_program() {
    let parser = parse_text(
        "
use Core.{
  Math.Random,
  Fmt
};

/// Say hello to a user.
public function SayHello(name: String): Outcome<String, String> {
    if name == \"\" {
      Error(\"empty name\")
    } else {
      message := Fmt.Sprintf(RandomFormat(), name);
      Ok(message)
    }
}

/// Generate a random format.
function RandomFormat(): String {
    formats := [
      \"Hello %v! How do you do?\",
      \"Great to see you, %v\",
      \"Hail %v! Well met!\"
    ];

    formats[Random.From(0..formats.Length())]  
}",
    );

    for statement in parser {
        assert!(statement.is_some() && !statement.has_errors())
    }
}

#[test]
fn parse_models() {
    // Simple.
    let mut parser = parse_text("model Person {}");
    let statement = parser.next().unwrap().unwrap();
    assert!(matches!(
        parser.scope_manager().lookaround("Person").unwrap().entry,
        ScopeEntry::Model(_)
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0].into(),
            body: ModelBody {
                properties: vec![],
                constructor: None,
                span: [1, 14, 1, 16].into(),
            },
            span: [1, 1, 1, 16].into(),
        }),
    );

    // With impl.
    let mut parser = parse_text("model TextBook implements Book {}");
    let statement = parser.next().unwrap().unwrap();
    assert!(matches!(
        parser.scope_manager().lookaround("TextBook").unwrap().entry,
        ScopeEntry::Model(m) if m.implementations.len() == 1
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0].into(),
            body: ModelBody {
                properties: vec![],
                constructor: None,
                span: [1, 32, 1, 34].into(),
            },
            span: [1, 1, 1, 34].into(),
        }),
    )
}

#[test]
fn parse_model_properties() {
    // Simple attribute.
    let mut parser = parse_text(
        "
    model Person {
        var name: String;
    }
    ",
    );
    let statement = parser.next().unwrap().unwrap();
    assert!(matches!(
        parser.scope_manager().lookaround("Person").unwrap().entry,
        ScopeEntry::Model(m) if m.attributes[0].name.name == "name" // haha.
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0].into(),
            body: ModelBody {
                properties: vec![ModelProperty {
                    index: 0,
                    _type: ModelPropertyType::Attribute,
                    span: [3, 9, 3, 26].into()
                }],
                constructor: None,
                span: [2, 18, 4, 6].into(),
            },
            span: [2, 5, 4, 6].into(),
        }),
    );

    // Public attribute.
    let mut parser = parse_text(
        "
    model Person {
       public var name: String;
    }
    ",
    );
    let statement = parser.next().unwrap().unwrap();
    assert!(matches!(
        parser.scope_manager().lookaround("Person").unwrap().entry,
        ScopeEntry::Model(m) if m.attributes[0].name.name == "name" && m.attributes[0].is_public // haha.
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0].into(),
            body: ModelBody {
                properties: vec![ModelProperty {
                    index: 0,
                    _type: ModelPropertyType::Attribute,
                    span: [3, 8, 3, 32].into()
                }],
                constructor: None,
                span: [2, 18, 4, 6].into(),
            },
            span: [2, 5, 4, 6].into(),
        }),
    );
}

#[test]
fn parse_model_functions() {
    // Simple function.
    let mut parser = parse_text(
        "
    model Person {
       function DoSomething() {
            
       }
    }
    ",
    );
    let statement = parser.next().unwrap().unwrap();
    assert!(matches!(
        parser.scope_manager().lookaround("Person").unwrap().entry,
        ScopeEntry::Model(m) if m.methods[0].name.name == "DoSomething"
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0].into(),
            body: ModelBody {
                properties: vec![ModelProperty {
                    index: 0,
                    _type: ModelPropertyType::Method {
                        body: Block {
                            statements: vec![],
                            span: [3, 31, 5, 9].into()
                        }
                    },
                    span: [3, 8, 5, 9].into()
                }],
                constructor: None,
                span: [2, 18, 6, 6].into(),
            },
            span: [2, 5, 6, 6].into(),
        }),
    );
}

#[test]
fn parse_static_method() {
    // Simple static function with variable.
    let mut parser = parse_text(
        "
    public model Person {
        var name: String;
        static function CreatePerson() {

        }
    }
    ",
    );
    let statement = parser.next().unwrap();
    let statement = statement.unwrap();
    assert!(matches!(
        parser.scope_manager().lookaround("Person").unwrap().entry,
        ScopeEntry::Model(m) if m.is_public && m.methods[0].name.name == "CreatePerson" && m.attributes[0].name.name == "name"
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0].into(),
            body: ModelBody {
                properties: vec![
                    ModelProperty {
                        index: 0,
                        _type: ModelPropertyType::Attribute,
                        span: [3, 9, 3, 26].into()
                    },
                    ModelProperty {
                        index: 0,
                        _type: ModelPropertyType::Method {
                            body: Block {
                                statements: vec![],
                                span: [4, 40, 6, 10].into()
                            }
                        },
                        span: [4, 9, 6, 10].into()
                    }
                ],
                constructor: None,
                span: [2, 25, 7, 6].into(),
            },
            span: [2, 5, 7, 6].into(),
        }),
    )
}

#[test]
fn parse_generic_params() {
    // Simple.
    let mut parser = parse_text(
        "function Find<T>
        (
            array: ArrayOf<T>, 
            predicate: fn(value: T): Boolean
        ): Maybe<T> {

        }",
    );
    let mut statement = parser.next().unwrap().unwrap();

    let mut scope_manager = parser.scope_manager();

    assert!(matches!(
        scope_manager.lookaround("Find").unwrap().entry,
        ScopeEntry::Function(f) if f.generic_params.as_ref().unwrap()[0].name.name == "T"
    ));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: ScopeAddress {
                scope_id: 0,
                entry_no: 0
            },
            body: Block {
                statements: vec![],
                span: [5, 21, 7, 10].into()
            },
            span: [1, 1, 7, 10].into()
        })
    );

    // With trait guards.
    parser = parse_text(
        "
    enum Result<T, E: Error> {
        Ok(T),
        Err(E)
    }",
    );
    statement = parser.next().unwrap().unwrap();

    scope_manager = parser.scope_manager();

    assert!(matches!(
        scope_manager.lookaround("Result").unwrap().entry,
        ScopeEntry::Enum(e) if {
            let param = &e.generic_params.as_ref().unwrap()[1];
            param.name.name == "E" && matches!(
                param.traits[0],
                TypeExpression::Discrete(ref d) if d.name.name == "Error"
            )
        }
    ));

    assert_eq!(
        statement,
        Statement::EnumDeclaration(EnumDeclaration {
            address: [0, 0].into(),
            span: [2, 5, 5, 6].into()
        })
    );

    // With default values.
    parser = parse_text(
        "
    model Stack<T: Sized + Nullable = Integer> {

    }
    ",
    );
    statement = parser.next().unwrap().unwrap();

    scope_manager = parser.scope_manager();
    assert!(matches!(
        scope_manager.lookaround("Stack").unwrap().entry,
        ScopeEntry::Model(m) if {
            let param = &m.generic_params.as_ref().unwrap()[0];
            param.name.name == "T" && param.traits.len() == 2 && matches!(
                param.default.as_ref().unwrap(),
                TypeExpression::Discrete(ref d) if d.name.name == "Integer"
            )
        }
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0].into(),
            body: ModelBody {
                properties: vec![],
                constructor: None,
                span: [2, 48, 4, 6].into()
            },
            span: [2, 5, 4, 6].into()
        })
    );
}

#[test]
fn parse_trait_declarations() {
    // Simple.
    let mut parser = parse_text("trait TraitName {}");
    let statement = parser.next().unwrap().unwrap();
    let mut scope_manager = parser.scope_manager();

    assert!(matches!(
        scope_manager.lookaround("TraitName").unwrap().entry,
        ScopeEntry::Trait(_)
    ));

    assert_eq!(
        statement,
        Statement::TraitDeclaration(TraitDeclaration {
            address: [0, 0].into(),
            body: TraitBody {
                properties: vec![],
                span: [1, 17, 1, 19].into()
            },
            span: [1, 1, 1, 19].into()
        })
    );

    // With one method signature.
    parser = parse_text(
        "
    public trait Addition {
        function Add(other: This): This;
    }
    ",
    );
    let statement = parser.next().unwrap().unwrap();
    scope_manager = parser.scope_manager();

    assert!(matches!(
        scope_manager.lookaround("Addition").unwrap().entry,
        ScopeEntry::Trait(t) if t.is_public && t.methods[0].name.name == "Add"
    ));

    assert_eq!(
        statement,
        Statement::TraitDeclaration(TraitDeclaration {
            address: [0, 0].into(),
            body: TraitBody {
                properties: vec![TraitProperty {
                    index: 0,
                    _type: whirl_ast::TraitPropertyType::Signature,
                    span: [3, 9, 3, 41].into()
                }],
                span: [2, 27, 4, 6].into()
            },
            span: [2, 5, 4, 6].into()
        })
    );

    // With a method signature and a method.
    parser = parse_text(
        "
    public trait Addition implements Core.General {
        function Add(other: This): This;
        function Add2(other: This) : This {
            // Stuff.
        }
    }
    ",
    );
    let statement = parser.next().unwrap().unwrap();
    scope_manager = parser.scope_manager();

    assert!(matches!(
        scope_manager.lookaround("Addition").unwrap().entry,
        ScopeEntry::Trait(t) if t.is_public
        && t.implementations.len() == 1
        && t.methods.len() == 2
    ));

    assert_eq!(
        statement,
        Statement::TraitDeclaration(TraitDeclaration {
            address: [0, 0].into(),
            body: TraitBody {
                properties: vec![
                    TraitProperty {
                        index: 0,
                        _type: whirl_ast::TraitPropertyType::Signature,
                        span: [3, 9, 3, 41].into()
                    },
                    TraitProperty {
                        index: 1,
                        _type: whirl_ast::TraitPropertyType::Method {
                            body: Block {
                                statements: vec![],
                                span: [4, 43, 6, 10].into()
                            }
                        },
                        span: [4, 9, 6, 10].into()
                    }
                ],
                span: [2, 51, 7, 6].into()
            },
            span: [2, 5, 7, 6].into()
        })
    );
}

#[test]
fn parse_while_statement() {
    let mut parser = parse_text("while true {}");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::WhileStatement(WhileStatement {
            condition: Expression::BooleanLiteral(WhirlBoolean {
                value: true,
                span: [1, 7, 1, 11].into()
            }),
            body: Block {
                statements: vec![],
                span: [1, 12, 1, 14].into()
            },
            span: [1, 1, 1, 14].into()
        })
    )
}

#[test]
fn parse_complex_program_2() {
    let parser = parse_text(
        "
 /// A last-in first-out data structure.
public model Stack<T> {
    var items: ArrayOf<T>;
    var capacity: Integer;

    new(capacity?: Integer) {
        this.items = [];
        this.capacity = capacity.UnwrapOr(Core.Math.INFINITY);
    }

    /// Dynamically change the number of items in the stack.
    /// If there are currently more items that the new capacity, then the top items will be removed until it matches the new capacity.
    public function SetCapacity(value: Integer) {
        while value < this.items.Length() {
            this.items.Pop();
        }
        this.capacity = value;
    }

    /// Returns the top value in the stack without removing it.
    public function Top(): Maybe<T> {
        this.items.Last()
    }

    /// Removes the last item from the stack and returns it, if it exists.
    public function Pop(): Maybe<T> {
        this.items.Pop()
    }

    /// Enter data into the stack.
    /// This method will fail if the size of the stack is already at maximum length.
    public function Push(data: T): Outcome<_, StackError> {
        if this.items.Length() == this.capacity {
            Err(new StackError(\"The stack is already full.\"))
        } else {
            this.items.Push(data);
            Ok(_)
        }
    }

    /// Returns the size of the stack.
    public function Size(): Integer {
        this.items.Length()
    }
}",
    );

    for statement in parser {
        assert!(statement.is_some() && !statement.has_errors());
    }
}
