#![cfg(test)]

use crate::parse_text;
use ast::{
    AccessExpr, ArrayExpr, AssignmentExpr, BinaryExpr, Block, BreakStatement, CallExpr,
    ConstantDeclaration, ContinueStatement, DiscreteType, Else, EnumDeclaration, Expression,
    ForStatement, FunctionDeclaration, FunctionExpr, Identifier, IfExpression, IndexExpr,
    InterfaceBody, InterfaceDeclaration, InterfaceProperty, LogicExpr, ModelBody, ModelDeclaration,
    ModelProperty, ModelPropertyType, ModuleDeclaration, Parameter, ReturnStatement, ScopeAddress,
    ScopeEntry, Span, Statement, TestDeclaration, ThisExpr, TypeDeclaration, TypeExpression,
    UnaryExpr, UpdateExpr, UpdateOperator, UseDeclaration, UsePath, UseTarget, VariableDeclaration,
    WhileStatement, WhirlBoolean, WhirlNumber, WhirlString,
};

#[test]
fn parsing_public_functions() {
    let mut parser = parse_text("public function CalculateCost(name) {}");

    let statement = parser.next().unwrap().unwrap();

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0, 0].into(),
            body: Block::empty(1, [1, 37, 1, 39].into()),
            span: [1, 1, 1, 39].into()
        })
    )
}

#[test]
fn parsing_functions_with_types() {
    // Normal types.
    let mut parser = parse_text("function GenerateHash(name: String, id: Int) {}");
    let statement = parser.next().unwrap().unwrap();

    assert!(parser
        .module_ambience()
        .lookaround("GenerateHash")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f) if matches!
            (
                &f.params[0].type_label,
                Some(TypeExpression::Discrete(d)) if d.name.name == "String"
            ) && matches!
            (
                &f.params[1].type_label,
                Some(TypeExpression::Discrete(d)) if d.name.name == "Int"
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0, 0].into(),
            body: Block::empty(1, [1, 46, 1, 48].into()),
            span: [1, 1, 1, 48].into()
        })
    )
}

#[test]
fn parsing_functions_with_member_types() {
    let mut parser = parse_text("function WriteFile(path: core.io.Path) {}");
    let statement = parser.next().unwrap().unwrap();

    assert!(parser
        .module_ambience()
        .lookaround("WriteFile")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f) if matches!
            (
                &f.params[0].type_label,
                Some(TypeExpression::Member(m)) if matches!(
                    &*m.namespace, TypeExpression::Member(_)
                )
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0, 0].into(),
            body: Block::empty(1, [1, 40, 1, 42].into()),
            span: [1, 1, 1, 42].into()
        })
    )
}

#[test]
fn parsing_functions_with_generic_types() {
    let mut parser = parse_text("function Combine(title: ArrayOf<String>) {}");
    let mut statement = parser.next().unwrap().unwrap();

    assert!(parser
        .module_ambience()
        .lookaround("Combine")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f) if matches!
            (
                &f.params[0].type_label,
                Some(TypeExpression::Discrete(d)) if
                    d.name.name == "ArrayOf" &&
                    d.generic_args.as_ref().unwrap().len() == 1
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0, 0].into(),
            body: Block::empty(1, [1, 42, 1, 44].into()),
            span: [1, 1, 1, 44].into()
        })
    );

    // Nested.
    parser = parse_text("function Flatten(nested: ArrayOf<ArrayOf<ArrayOf<String>>>) {}");
    statement = parser.next().unwrap().unwrap();

    assert!(parser
        .module_ambience()
        .lookaround("Flatten")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f)
            if matches!
            (
                &f.params[0].type_label,
                Some(TypeExpression::Discrete(d)) if
                    d.name.name == "ArrayOf" &&
                    d.generic_args.as_ref().unwrap().len() == 1
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0, 0].into(),
            body: Block::empty(1, [1, 61, 1, 63].into()),
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
    //     parser.module_ambience().lookaround("Find").unwrap().entry
    // );

    assert!(parser
        .module_ambience()
        .lookaround("Find")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f) if matches!
            (
                &f.params[0].type_label,
                Some(TypeExpression::Functional(f)) if matches!
                {
                    f.params[0].type_label,
                    Some(TypeExpression::Discrete(_))
                } && f.return_type.is_some()
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0, 0].into(),
            body: Block::empty(1, [1, 52, 1, 54].into()),
            span: [1, 1, 1, 54].into()
        })
    );
}

#[test]
fn parse_function_types_with_arrow() {
    let mut parser = parse_text("function Find(predicate: fn(value: Name) -> Boolean) {}");
    let statement = parser.next().unwrap().unwrap();

    // println!(
    //     "{:?}",
    //     parser.module_ambience().lookaround("Find").unwrap().entry
    // );

    assert!(parser
        .module_ambience()
        .lookaround("Find")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f) if matches!
            (
                &f.params[0].type_label,
                Some(TypeExpression::Functional(f)) if matches!
                {
                    f.params[0].type_label,
                    Some(TypeExpression::Discrete(_))
                } && f.return_type.is_some()
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0, 0].into(),
            body: Block::empty(1, [1, 54, 1, 56].into()),
            span: [1, 1, 1, 56].into()
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
    //         .module_ambience()
    //         .lookaround("EatHealthy")
    //         .unwrap()
    //         .entry
    // );

    assert!(parser
        .module_ambience()
        .lookaround("EatHealthy")
        .is_some_and(|search| matches!(
            search.entry,
            ScopeEntry::Function(f) if matches!
            (
                &f.params[0].type_label,
                Some(TypeExpression::Union(u)) if u.types.len() == 6
            )
        )));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: [0, 0, 0].into(),
            body: Block::empty(1, [10, 9, 10, 11].into()),
            span: [2, 9, 10, 11].into()
        })
    );
}

#[test]
fn parsing_conditional_types() {
    let mut parser = parse_text("type A = if A is A<B> B<C> else B<A>;");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::TypeDeclaration(TypeDeclaration {
            address: ScopeAddress::from([0, 0, 0]),
            span: Span::from([1, 1, 1, 38])
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
            address: ScopeAddress::from([0, 0, 0]),
            body: Block::empty(1, Span::from([1, 20, 1, 22])),
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
    let mut module_ambience = parser.module_ambience();

    assert!(module_ambience.is_in_global_scope());
    assert_eq!(module_ambience.scope_len(), 3);
    assert!(module_ambience.lookdown("SayHelloAgain").is_some());

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: ScopeAddress::from([0, 0, 0]),
            body: Block {
                scope_id: 1,
                statements: vec![Statement::FunctionDeclaration(FunctionDeclaration {
                    address: ScopeAddress::from([0, 1, 0]),
                    body: Block::empty(2, Span::from([3, 34, 5, 10])),
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
    module_ambience = parser.module_ambience();

    assert!(module_ambience.lookaround("GreetUser").is_some());

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: ScopeAddress::from([0, 0, 0]),
            body: Block {
                scope_id: 1,
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
    let module_ambience = parser.module_ambience();

    assert!(module_ambience
        .lookaround("GreetUser")
        .is_some_and(|search| matches!(search.entry, ast::ScopeEntry::Function(f) if f.is_async)));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: ScopeAddress::from([0, 0, 0]),
            body: Block {
                scope_id: 1,
                statements: vec![],
                span: Span::from([1, 36, 1, 38])
            },
            span: Span::from([1, 1, 1, 38])
        })
    );
}

#[test]
fn parse_function_with_nested_generics() {
    let mut parser = parse_text("function Collect<T implements Collectible<Item>>(): T {}");

    let statement = parser.next().unwrap().unwrap();
    let module_ambience = parser.module_ambience();

    assert!(module_ambience
        .lookaround("Collect")
        .is_some_and(|search| matches!(search.entry, ast::ScopeEntry::Function(_))));

    // println!("{:#?}", module_ambience);

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: ScopeAddress::from([0, 0, 0]),
            body: Block {
                scope_id: 1,
                statements: vec![],
                span: Span::from([1, 55, 1, 57])
            },
            span: Span::from([1, 1, 1, 57])
        })
    );
}

#[test]
fn parsing_type_declarations() {
    let mut parser = parse_text("public type Number = SignedInt | UnsignedInt | Float;");

    let statement = parser.next().unwrap().unwrap();
    let module_ambience = parser.module_ambience();

    assert!(module_ambience
        .lookaround("Number")
        .is_some_and(|search| matches!(search.entry, ast::ScopeEntry::Type(t) if t.is_public)));

    assert_eq!(
        statement,
        Statement::TypeDeclaration(TypeDeclaration {
            address: ScopeAddress::from([0, 0, 0]),
            span: Span::from([1, 1, 1, 54])
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
    let module_ambience = parser.module_ambience();

    assert!(module_ambience
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
    let module_ambience = parser.module_ambience();

    assert!(module_ambience
        .lookaround("Self")
        .is_some_and(|search| matches!(
            search.entry, ast::ScopeEntry::Type(t) if matches!(
                t.value,
                TypeExpression::This { .. }
            )
        )));

    assert_eq!(
        statement,
        Statement::TypeDeclaration(TypeDeclaration {
            address: ScopeAddress::from([0, 0, 0]),
            span: Span::from([1, 1, 1, 18])
        })
    );
}

#[test]
fn parse_constrained_type() {
    let mut parser = parse_text("type A = B|=C implements D;");
    let statement = parser.next().unwrap().unwrap();
    let module_ambience = parser.module_ambience();

    assert!(module_ambience
        .lookaround("A")
        .is_some_and(|search| matches!(
            search.entry, ast::ScopeEntry::Type(t) if matches!(
                t.value,
                TypeExpression::Constraint{ .. }
            )
        )));

    assert_eq!(
        statement,
        Statement::TypeDeclaration(TypeDeclaration {
            address: ScopeAddress::from([0, 0, 0]),
            span: Span::from([1, 1, 1, 28])
        })
    );

    // with brackets.
    let mut parser = parse_text("type A = B|=(C implements D|=A is B);");
    let statement = parser.next().unwrap().unwrap();
    let module_ambience = parser.module_ambience();

    assert!(module_ambience
        .lookaround("A")
        .is_some_and(|search| matches!(
            search.entry, ast::ScopeEntry::Type(t) if matches!(
                t.value,
                TypeExpression::Constraint{ .. }
            )
        )));

    assert_eq!(
        statement,
        Statement::TypeDeclaration(TypeDeclaration {
            address: ScopeAddress::from([0, 0, 0]),
            span: Span::from([1, 1, 1, 38])
        })
    );

    // binary clause.
    let mut parser =
        parse_text("type A = B|=(C implements D|=A is B and C is D<G> and E implements F);");
    let statement = parser.next().unwrap().unwrap();
    let module_ambience = parser.module_ambience();

    assert!(module_ambience
        .lookaround("A")
        .is_some_and(|search| matches!(
            search.entry, ast::ScopeEntry::Type(t) if matches!(
                t.value,
                TypeExpression::Constraint{ .. }
            )
        )));

    assert_eq!(
        statement,
        Statement::TypeDeclaration(TypeDeclaration {
            address: ScopeAddress::from([0, 0, 0]),
            span: Span::from([1, 1, 1, 71])
        })
    );
}

#[test]
fn parse_array_type() {
    let mut parser = parse_text("type Bytes = []UInt8;");
    let statement = parser.next().unwrap().unwrap();
    let module_ambience = parser.module_ambience();

    assert!(module_ambience
        .lookaround("Bytes")
        .is_some_and(|search| matches!(
            search.entry, ast::ScopeEntry::Type(t) if matches!(
                t.value,
                TypeExpression::Array(_)
            )
        )));

    assert_eq!(
        statement,
        Statement::TypeDeclaration(TypeDeclaration {
            address: ScopeAddress::from([0, 0, 0]),
            span: Span::from([1, 1, 1, 22])
        })
    );
}

#[test]
fn parse_grouped_type() {
    let mut parser = parse_text("type Bytes = [](UInt8 | UInt16);");
    let statement = parser.next().unwrap().unwrap();
    let module_ambience = parser.module_ambience();

    assert!(module_ambience
        .lookaround("Bytes")
        .is_some_and(|search| matches!(
            search.entry, ast::ScopeEntry::Type(t) if matches!(
                t.value,
                TypeExpression::Array(_)
            )
        )));

    assert_eq!(
        statement,
        Statement::TypeDeclaration(TypeDeclaration {
            address: ScopeAddress::from([0, 0, 0]),
            span: Span::from([1, 1, 1, 33])
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
            body: Block::empty(1, [1, 19, 1, 21].into()),
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

    let mut statement = parser.next().unwrap().expect(|err| format!("{err:?}"));

    let mut module_ambience = parser.module_ambience();

    assert!(module_ambience
        .lookaround("Scope")
        .is_some_and(|search| matches!(
            search.entry, ast::ScopeEntry::Enum(e) if matches!(
                e.variants[1].name.name.as_str(),
                "Public"
            )
        )));

    assert_eq!(
        statement,
        Statement::EnumDeclaration(EnumDeclaration {
            address: [0, 0, 0].into(),
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

    module_ambience = parser.module_ambience();

    assert!(module_ambience
        .lookaround("Node")
        .is_some_and(|search| matches!(
            search.entry, ast::ScopeEntry::Enum(e) if matches!(
                e.variants[1].name.name.as_str(),
                "Child"
            ) && e.variants[1].tagged_types.len() == 1
        )));

    assert_eq!(
        statement,
        Statement::EnumDeclaration(EnumDeclaration {
            address: [0, 0, 0].into(),
            span: Span::from([2, 6, 5, 6])
        })
    );

    // Multi-Tagged Enums.
    parser = parse_text(
        "
     enum Position {
        TwoDimensional(Int, Int),
        ThreeDimensional(Int, Int, Int)
    }",
    );

    statement = parser.next().unwrap().unwrap();

    module_ambience = parser.module_ambience();

    assert!(module_ambience
        .lookaround("Position")
        .is_some_and(|search| matches!(
            search.entry, ast::ScopeEntry::Enum(e) if matches!(
                e.variants[1].name.name.as_str(),
                "ThreeDimensional"
            ) && e.variants[1].tagged_types.len() == 3
        )));

    assert_eq!(
        statement,
        Statement::EnumDeclaration(EnumDeclaration {
            address: [0, 0, 0].into(),
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
            addresses: vec![[0, 0, 0].into()],
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
            addresses: vec![[0, 0, 0].into()],
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
    parser = parse_text("use core.Math;");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::UseDeclaration(UseDeclaration {
            addresses: vec![[0, 0, 0].into()],
            target: UseTarget {
                name: Identifier {
                    name: format!("core"),
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
            addresses: vec![[0, 0, 0].into()],
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
    let statement = parser.next().unwrap().unwrap();

    if let Statement::UseDeclaration(usedecl) = &statement {
        assert!(usedecl.target.scatter().len() == usedecl.target.leaves().len())
    }
    assert_eq!(
        statement,
        Statement::UseDeclaration(UseDeclaration {
            addresses: vec![[0, 0, 0].into(), [0, 0, 1].into()],
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
    parser.debug_allow_global_expressions = true;
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
    parser.debug_allow_global_expressions = true;
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
    parser.debug_allow_global_expressions = true;
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
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::StringLiteral(ast::WhirlString {
            value: format!("Hello, world!"),
            span: [1, 1, 1, 16].into()
        }))
    );
}

#[test]
fn parse_fn_expressions() {
    let mut parser = parse_text("fn (a: Number): Number a");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::FnExpr(Box::new(FunctionExpr {
            is_async: false,
            generic_params: None,
            params: Some(vec![Parameter {
                name: Identifier {
                    name: format!("a"),
                    span: [1, 5, 1, 6].into()
                },
                info: None,
                type_label: Some(TypeExpression::Discrete(DiscreteType {
                    name: Identifier {
                        name: format!("Number"),
                        span: [1, 8, 1, 14].into()
                    },
                    generic_args: None,

                    span: [1, 8, 1, 14].into()
                })),
                is_optional: false,
                span: [1, 5, 1, 14].into()
            }]),
            return_type: Some(TypeExpression::Discrete(DiscreteType {
                name: Identifier {
                    name: format!("Number"),
                    span: [1, 17, 1, 23].into()
                },
                generic_args: None,

                span: [1, 17, 1, 23].into()
            })),
            body: Expression::Identifier(Identifier {
                name: format!("a"),
                span: [1, 24, 1, 25].into()
            }),
            span: [1, 1, 1, 25].into()
        })))
    );
}

#[test]
fn parse_async_fn_expression() {
    let mut parser = parse_text("(async fn (a) a)");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{err:?}")),
        Statement::FreeExpression(Expression::FnExpr(Box::new(FunctionExpr {
            is_async: true,
            generic_params: None,
            params: Some(vec![Parameter {
                name: Identifier {
                    name: format!("a"),
                    span: [1, 12, 1, 13].into()
                },
                info: None,
                type_label: None,
                is_optional: false,
                span: [1, 12, 1, 13].into()
            }]),
            return_type: None,
            body: Expression::Identifier(Identifier {
                name: format!("a"),
                span: [1, 15, 1, 16].into()
            }),
            span: [1, 2, 1, 16].into()
        })))
    );
}

#[test]
fn parse_if_expressions() {
    // Without else.
    let mut parser = parse_text("if IsLegal() { \"Come on in\" }");
    parser.debug_allow_global_expressions = true;
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
                scope_id: 1,
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
    parser.debug_allow_global_expressions = true;
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
                scope_id: 1,
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
                    scope_id: 2,
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
    parser.debug_allow_global_expressions = true;

    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::ShorthandVariableDeclaration(ast::ShorthandVariableDeclaration {
            address: [0, 0, 0].into(),
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

    let module_ambience = parser.module_ambience();

    assert!(matches!(
        module_ambience.lookaround("message").unwrap().entry,
        ScopeEntry::ShorthandVariable(ast::ShorthandVariableSignature {name,..}) if name.name == format!("message")));

    // With type
    parser = parse_text("array: ArrayOf<String> := MakeArray();");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::ShorthandVariableDeclaration(ast::ShorthandVariableDeclaration {
            address: [0, 0, 0].into(),
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
fn parse_constants() {
    let mut parser = parse_text("const NAME: String = \"Sefunmi\";");
    // parser.debug_allow_global_expressions = true;

    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::ConstantDeclaration(ConstantDeclaration {
            address: [0, 0, 0].into(),
            value: Expression::StringLiteral(WhirlString {
                value: format!("Sefunmi"),
                span: [1, 22, 1, 31].into()
            }),
            span: [1, 1, 1, 32].into()
        })
    );
}

#[test]
fn parse_arrays() {
    let mut parser = parse_text("[a, b, c, d]");
    parser.debug_allow_global_expressions = true;
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
    parser.debug_allow_global_expressions = true;
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

// #[test]
// fn parse_new_expression() {
//     let mut parser = parse_text("new Stack()");
//     parser.debug_allow_global_expressions = true;
//     assert_eq!(
//         parser.next().unwrap().unwrap(),
//         Statement::FreeExpression(Expression::CallExpr(Box::new(CallExpr {
//             caller: Expression::NewExpr(Box::new(NewExpr {
//                 value: Expression::Identifier(Identifier {
//                     name: format!("Stack"),
//                     span: [1, 5, 1, 10].into()
//                 }),
//                 span: [1, 1, 1, 10].into()
//             })),
//             arguments: vec![],
//             span: [1, 1, 1, 12].into()
//         })))
//     )
// }

#[test]
fn parse_this_expression() {
    let mut parser = parse_text("this");
    parser.debug_allow_global_expressions = true;
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
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::BinaryExpr(Box::new(BinaryExpr {
            left: Expression::BinaryExpr(Box::new(BinaryExpr {
                left: Expression::NumberLiteral(WhirlNumber {
                    value: ast::Number::Decimal(format!("2")),
                    span: [1, 1, 1, 2].into()
                }),
                operator: ast::BinOperator::Add,
                right: Expression::NumberLiteral(WhirlNumber {
                    value: ast::Number::Decimal(format!("2")),
                    span: [1, 5, 1, 6].into()
                }),
                span: [1, 1, 1, 6].into()
            })),
            operator: ast::BinOperator::Add,
            right: Expression::NumberLiteral(WhirlNumber {
                value: ast::Number::Decimal(format!("2")),
                span: [1, 9, 1, 10].into()
            }),
            span: [1, 1, 1, 10].into()
        })))
    );

    // Different operators.
    parser = parse_text("2 + 3 * 4");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::BinaryExpr(Box::new(BinaryExpr {
            left: Expression::NumberLiteral(WhirlNumber {
                value: ast::Number::Decimal(format!("2")),
                span: [1, 1, 1, 2].into()
            }),
            operator: ast::BinOperator::Add,
            right: Expression::BinaryExpr(Box::new(BinaryExpr {
                left: Expression::NumberLiteral(WhirlNumber {
                    value: ast::Number::Decimal(format!("3")),
                    span: [1, 5, 1, 6].into()
                }),
                operator: ast::BinOperator::Multiply,
                right: Expression::NumberLiteral(WhirlNumber {
                    value: ast::Number::Decimal(format!("4")),
                    span: [1, 9, 1, 10].into()
                }),
                span: [1, 5, 1, 10].into()
            })),
            span: [1, 1, 1, 10].into()
        })))
    );

    // Raised to operator.
    parser = parse_text("2 ^ 3 ^ 4");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::BinaryExpr(Box::new(BinaryExpr {
            left: Expression::NumberLiteral(WhirlNumber {
                value: ast::Number::Decimal(format!("2")),
                span: [1, 1, 1, 2].into()
            }),
            operator: ast::BinOperator::PowerOf,
            right: Expression::BinaryExpr(Box::new(BinaryExpr {
                left: Expression::NumberLiteral(WhirlNumber {
                    value: ast::Number::Decimal(format!("3")),
                    span: [1, 5, 1, 6].into()
                }),
                operator: ast::BinOperator::PowerOf,
                right: Expression::NumberLiteral(WhirlNumber {
                    value: ast::Number::Decimal(format!("4")),
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
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::LogicExpr(Box::new(LogicExpr {
            left: Expression::Identifier(Identifier {
                name: format!("isTrue"),
                span: [1, 1, 1, 7].into()
            }),
            operator: ast::LogicOperator::Or,
            right: Expression::Identifier(Identifier {
                name: format!("isFalse"),
                span: [1, 11, 1, 18].into()
            }),
            span: [1, 1, 1, 18].into()
        })))
    );

    // literal and multiple.
    parser = parse_text("isTrue || isFalse and isTrue");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::LogicExpr(Box::new(LogicExpr {
            left: Expression::LogicExpr(Box::new(LogicExpr {
                left: Expression::Identifier(Identifier {
                    name: format!("isTrue"),
                    span: [1, 1, 1, 7].into()
                }),
                operator: ast::LogicOperator::Or,
                right: Expression::Identifier(Identifier {
                    name: format!("isFalse"),
                    span: [1, 11, 1, 18].into()
                }),
                span: [1, 1, 1, 18].into()
            })),
            operator: ast::LogicOperator::AndLiteral,
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
    let mut parser = parse_text("core.fmt");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::AccessExpr(Box::new(AccessExpr {
            object: Expression::Identifier(Identifier {
                name: format!("core"),
                span: [1, 1, 1, 5].into()
            }),
            property: Expression::Identifier(Identifier {
                name: format!("fmt"),
                span: [1, 6, 1, 9].into()
            }),
            span: [1, 1, 1, 9].into()
        })))
    );

    // nested
    parser = parse_text("core.Fmt.Println()");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::CallExpr(Box::new(CallExpr {
            caller: Expression::AccessExpr(Box::new(AccessExpr {
                object: Expression::AccessExpr(Box::new(AccessExpr {
                    object: Expression::Identifier(Identifier {
                        name: format!("core"),
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
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::AssignmentExpr(Box::new(AssignmentExpr {
            left: Expression::Identifier(Identifier {
                name: format!("a"),
                span: [1, 1, 1, 2].into()
            }),
            operator: ast::AssignOperator::Assign,
            right: Expression::Identifier(Identifier {
                name: format!("b"),
                span: [1, 5, 1, 6].into()
            }),
            span: [1, 1, 1, 6].into()
        })))
    );

    // nested.
    let mut parser = parse_text("a = b = c");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::AssignmentExpr(Box::new(AssignmentExpr {
            left: Expression::Identifier(Identifier {
                name: format!("a"),
                span: [1, 1, 1, 2].into()
            }),
            operator: ast::AssignOperator::Assign,
            right: Expression::AssignmentExpr(Box::new(AssignmentExpr {
                left: Expression::Identifier(Identifier {
                    name: format!("b"),
                    span: [1, 5, 1, 6].into()
                }),
                operator: ast::AssignOperator::Assign,
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
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::UnaryExpr(Box::new(UnaryExpr {
            operator: ast::UnaryOperator::Negation,
            operand: Expression::Identifier(Identifier {
                name: format!("a"),
                span: [1, 2, 1, 3].into()
            }),
            span: [1, 1, 1, 3].into()
        })))
    );
}

#[test]
fn parse_update_expression() {
    let mut parser = parse_text("a! + a?");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::BinaryExpr(Box::new(BinaryExpr {
            left: Expression::UpdateExpr(Box::new(UpdateExpr {
                operator: UpdateOperator::Assert,
                operand: Expression::Identifier(Identifier {
                    name: format!("a"),
                    span: [1, 1, 1, 2].into()
                }),
                span: [1, 1, 1, 3].into()
            })),
            operator: ast::BinOperator::Add,
            right: Expression::UpdateExpr(Box::new(UpdateExpr {
                operator: UpdateOperator::TryFrom,
                operand: Expression::Identifier(Identifier {
                    name: format!("a"),
                    span: [1, 6, 1, 7].into()
                }),
                span: [1, 6, 1, 8].into()
            })),
            span: [1, 1, 1, 8].into()
        })))
    );
}

#[test]
fn parse_complex_program() {
    let parser = parse_text(
        "
use core.{
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
        parser.module_ambience().lookaround("Person").unwrap().entry,
        ScopeEntry::Model(_)
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0, 0].into(),
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
        parser.module_ambience().lookaround("TextBook").unwrap().entry,
        ScopeEntry::Model(m) if m.implementations.len() == 1
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0, 0].into(),
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
        parser.module_ambience().lookaround("Person").unwrap().entry,
        ScopeEntry::Model(m) if m.attributes[0].name.name == "name" // haha.
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0, 0].into(),
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
        parser.module_ambience().lookaround("Person").unwrap().entry,
        ScopeEntry::Model(m) if m.attributes[0].name.name == "name" && m.attributes[0].is_public // haha.
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0, 0].into(),
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
        parser.module_ambience().lookaround("Person").unwrap().entry,
        ScopeEntry::Model(m) if m.methods[0].name.name == "DoSomething"
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0, 0].into(),
            body: ModelBody {
                properties: vec![ModelProperty {
                    index: 0,
                    _type: ModelPropertyType::Method {
                        body: Block {
                            scope_id: 1,
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
        parser.module_ambience().lookaround("Person").unwrap().entry,
        ScopeEntry::Model(m) if m.is_public && m.methods[0].name.name == "CreatePerson" && m.attributes[0].name.name == "name"
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0, 0].into(),
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
                                scope_id: 1,
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

    let mut module_ambience = parser.module_ambience();

    assert!(matches!(
        module_ambience.lookaround("Find").unwrap().entry,
        ScopeEntry::Function(f) if f.generic_params.as_ref().unwrap()[0].name.name == "T"
    ));

    assert_eq!(
        statement,
        Statement::FunctionDeclaration(FunctionDeclaration {
            address: ScopeAddress {
                module_id: 0,
                scope_id: 0,
                entry_no: 0
            },
            body: Block {
                scope_id: 1,
                statements: vec![],
                span: [5, 21, 7, 10].into()
            },
            span: [1, 1, 7, 10].into()
        })
    );

    // With interface guards.
    parser = parse_text(
        "
    enum Result<T, E implements Error> {
        Ok(T),
        Err(E)
    }",
    );
    statement = parser.next().unwrap().unwrap();

    module_ambience = parser.module_ambience();

    assert!(matches!(
        module_ambience.lookaround("Result").unwrap().entry,
        ScopeEntry::Enum(e) if {
            let param = &e.generic_params.as_ref().unwrap()[1];
            param.name.name == "E" && matches!(
                param.interfaces[0],
                TypeExpression::Discrete(ref d) if d.name.name == "Error"
            )
        }
    ));

    assert_eq!(
        statement,
        Statement::EnumDeclaration(EnumDeclaration {
            address: [0, 0, 0].into(),
            span: [2, 5, 5, 6].into()
        })
    );

    // With default values.
    parser = parse_text(
        "
    model Stack<T implements Sized + Nullable = Int> {

    }
    ",
    );
    statement = parser.next().unwrap().unwrap();

    module_ambience = parser.module_ambience();
    assert!(matches!(
        module_ambience.lookaround("Stack").unwrap().entry,
        ScopeEntry::Model(m) if {
            let param = &m.generic_params.as_ref().unwrap()[0];
            param.name.name == "T" && param.interfaces.len() == 2 && matches!(
                param.default.as_ref().unwrap(),
                TypeExpression::Discrete(ref d) if d.name.name == "Int"
            )
        }
    ));

    assert_eq!(
        statement,
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0, 0].into(),
            body: ModelBody {
                properties: vec![],
                constructor: None,
                span: [2, 54, 4, 6].into()
            },
            span: [2, 5, 4, 6].into()
        })
    );
}

#[test]
fn parse_interface_declarations() {
    // Simple.
    let mut parser = parse_text("interface InterfaceName {}");
    let statement = parser.next().unwrap().unwrap();
    let mut module_ambience = parser.module_ambience();

    assert!(matches!(
        module_ambience.lookaround("InterfaceName").unwrap().entry,
        ScopeEntry::Interface(_)
    ));

    assert_eq!(
        statement,
        Statement::InterfaceDeclaration(InterfaceDeclaration {
            address: [0, 0, 0].into(),
            body: InterfaceBody {
                properties: vec![],
                span: [1, 25, 1, 27].into()
            },
            span: [1, 1, 1, 27].into()
        })
    );

    // With one method signature.
    parser = parse_text(
        "
    public interface Addition {
        function Add(other: This): This;
    }
    ",
    );
    let statement = parser.next().unwrap().unwrap();
    module_ambience = parser.module_ambience();

    assert!(matches!(
        module_ambience.lookaround("Addition").unwrap().entry,
        ScopeEntry::Interface(t) if t.is_public && t.methods[0].name.name == "Add"
    ));

    assert_eq!(
        statement,
        Statement::InterfaceDeclaration(InterfaceDeclaration {
            address: [0, 0, 0].into(),
            body: InterfaceBody {
                properties: vec![InterfaceProperty {
                    index: 0,
                    _type: ast::InterfacePropertyType::Signature,
                    span: [3, 9, 3, 41].into()
                }],
                span: [2, 31, 4, 6].into()
            },
            span: [2, 5, 4, 6].into()
        })
    );

    // With a method signature and a method.
    parser = parse_text(
        "
    public interface Addition implements core.General {
        function Add(other: This): This;
        function Add2(other: This) : This {
            // Stuff.
        }
    }
    ",
    );
    let statement = parser.next().unwrap().unwrap();
    module_ambience = parser.module_ambience();

    assert!(matches!(
        module_ambience.lookaround("Addition").unwrap().entry,
        ScopeEntry::Interface(t) if t.is_public
        && t.implementations.len() == 1
        && t.methods.len() == 2
    ));

    assert_eq!(
        statement,
        Statement::InterfaceDeclaration(InterfaceDeclaration {
            address: [0, 0, 0].into(),
            body: InterfaceBody {
                properties: vec![
                    InterfaceProperty {
                        index: 0,
                        _type: ast::InterfacePropertyType::Signature,
                        span: [3, 9, 3, 41].into()
                    },
                    InterfaceProperty {
                        index: 1,
                        _type: ast::InterfacePropertyType::Method {
                            body: Block {
                                scope_id: 1,
                                statements: vec![],
                                span: [4, 43, 6, 10].into()
                            }
                        },
                        span: [4, 9, 6, 10].into()
                    }
                ],
                span: [2, 55, 7, 6].into()
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
                scope_id: 1,
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
    var capacity: Int;

    new(capacity?: Int) {
        this.items = [];
        this.capacity = capacity.UnwrapOr(core.Math.INFINITY);
    }

    /// Dynamically change the number of items in the stack.
    /// If there are currently more items that the new capacity, then the top items will be removed until it matches the new capacity.
    public function SetCapacity(value: Int) {
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
            Err(StackError(\"The stack is already full.\"))
        } else {
            this.items.Push(data);
            Ok(_)
        }
    }

    /// Returns the size of the stack.
    public function Size(): Int {
        this.items.Length()
    }
}",
    );

    for statement in parser {
        assert!(statement.is_some() && !statement.has_errors());
    }
}

#[test]
fn parse_module_declaration() {
    let mut parser = parse_text("module Structures;");
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::ModuleDeclaration(ModuleDeclaration {
            span: [1, 1, 1, 19].into()
        })
    );
    assert_eq!(
        parser.module_ambience().get_module_name().unwrap(),
        "Structures"
    )
}

#[test]
fn parse_return_statement() {
    // Without expression.
    let mut parser = parse_text("return;");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::ReturnStatement(ReturnStatement {
            value: None,
            span: [1, 1, 1, 8].into()
        })
    );

    // With expression.
    let mut parser = parse_text("fn () {return a;}");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::FreeExpression(Expression::FnExpr(Box::new(FunctionExpr {
            is_async: false,
            generic_params: None,
            params: Some(vec![]),
            return_type: None,
            body: Expression::BlockExpr(Block {
                scope_id: 1,
                statements: vec![Statement::ReturnStatement(ReturnStatement {
                    value: Some(Expression::Identifier(Identifier {
                        name: format!("a"),
                        span: [1, 15, 1, 16].into()
                    })),
                    span: [1, 8, 1, 17].into()
                })],
                span: [1, 7, 1, 18].into()
            }),
            span: [1, 1, 1, 18].into()
        })))
    )
}

#[test]
fn parse_model_with_interface_impl() {
    let mut parser = parse_text(
        "
    public model Person implements Greeting {
        function [Greeting.SayHello]() {
            // Code.
        }
    }    ",
    );
    assert_eq!(
        parser.next().unwrap().unwrap(),
        Statement::ModelDeclaration(ModelDeclaration {
            address: [0, 0, 0].into(),
            body: ModelBody {
                properties: vec![ModelProperty {
                    index: 0,
                    _type: ModelPropertyType::InterfaceImpl {
                        interface_target: vec![
                            DiscreteType {
                                name: Identifier {
                                    name: format!("Greeting"),
                                    span: [3, 19, 3, 27].into()
                                },
                                generic_args: None,
                                span: [3, 19, 3, 27].into()
                            },
                            DiscreteType {
                                name: Identifier {
                                    name: format!("SayHello"),
                                    span: [3, 28, 3, 36].into()
                                },
                                generic_args: None,

                                span: [3, 28, 3, 36].into()
                            }
                        ],
                        body: Block {
                            scope_id: 1,
                            statements: vec![],
                            span: [3, 40, 5, 10].into()
                        }
                    },
                    span: [3, 9, 5, 10].into()
                }],
                constructor: None,
                span: [2, 45, 6, 6].into()
            },
            span: [2, 5, 6, 6].into()
        })
    )
}

#[test]
fn parse_variable_declaration() {
    // simple.
    let mut parser = parse_text("var name = \"Sefunmi\";");
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{:?}", err)),
        Statement::VariableDeclaration(VariableDeclaration {
            addresses: vec![[0, 0, 0].into()],
            value: Some(Expression::StringLiteral(WhirlString {
                value: String::from("Sefunmi"),
                span: [1, 12, 1, 21].into()
            })),
            span: [1, 1, 1, 22].into()
        })
    );
    assert!(parser.module_ambience().lookaround("name").is_some());

    // without initializer.
    let mut parser = parse_text("var something;");
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{:?}", err)),
        Statement::VariableDeclaration(VariableDeclaration {
            addresses: vec![[0, 0, 0].into()],
            value: None,
            span: [1, 1, 1, 15].into()
        })
    );
    assert!(parser.module_ambience().lookaround("something").is_some());

    // with type.
    let mut parser = parse_text("var something: SomeType;");
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{:?}", err)),
        Statement::VariableDeclaration(VariableDeclaration {
            addresses: vec![[0, 0, 0].into()],
            value: None,
            span: [1, 1, 1, 25].into()
        })
    );
    assert!(parser.module_ambience().lookaround("something").is_some());

    // destructured model.
    let mut parser = parse_text("var { prop1, prop2 as prop3 };");
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{:?}", err)),
        Statement::VariableDeclaration(VariableDeclaration {
            addresses: vec![[0, 0, 0].into(), [0, 0, 1].into()],
            value: None,
            span: [1, 1, 1, 31].into()
        })
    );
    assert!(parser.module_ambience().lookaround("prop1").is_some());
    assert!(parser.module_ambience().lookaround("prop3").is_some());

    // destructured array.
    let mut parser = parse_text("var [ prop1, prop2 ];");
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{:?}", err)),
        Statement::VariableDeclaration(VariableDeclaration {
            addresses: vec![[0, 0, 0].into(), [0, 0, 1].into()],
            value: None,
            span: [1, 1, 1, 22].into()
        })
    );
    assert!(parser.module_ambience().lookaround("prop1").is_some());
    assert!(parser.module_ambience().lookaround("prop2").is_some());
}

#[test]
fn parse_for_statement() {
    // Simple.
    let mut parser = parse_text("for item in list {} ");
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{:?}", err)),
        Statement::ForStatement(ForStatement {
            items: vec![[0, 1, 0].into()],
            iterator: Expression::Identifier(Identifier {
                name: format!("list"),
                span: [1, 13, 1, 17].into()
            }),
            label: None,
            body: Block {
                scope_id: 1,
                statements: vec![],
                span: [1, 18, 1, 20].into()
            },
            span: [1, 1, 1, 20].into()
        })
    );

    // Destructured
    let mut parser = parse_text("for { prop1, prop2 as prop3 } in list {} ");
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{:?}", err)),
        Statement::ForStatement(ForStatement {
            items: vec![[0, 1, 0].into(), [0, 1, 1].into()],
            iterator: Expression::Identifier(Identifier {
                name: format!("list"),
                span: [1, 34, 1, 38].into()
            }),
            label: None,
            body: Block {
                scope_id: 1,
                statements: vec![],
                span: [1, 39, 1, 41].into()
            },
            span: [1, 1, 1, 41].into()
        })
    );

    // With label.
    let mut parser = parse_text("for item in list as outerLoop {} ");
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{:?}", err)),
        Statement::ForStatement(ForStatement {
            items: vec![[0, 1, 0].into()],
            iterator: Expression::Identifier(Identifier {
                name: format!("list"),
                span: [1, 13, 1, 17].into()
            }),
            label: Some([0, 1, 1].into()),
            body: Block {
                scope_id: 1,
                statements: vec![],
                span: [1, 31, 1, 33].into()
            },
            span: [1, 1, 1, 33].into()
        })
    );
}

#[test]
fn parse_continue_statement() {
    let mut parser = parse_text("continue;");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{:?}", err)),
        Statement::ContinueStatement(ContinueStatement {
            label: None,
            span: [1, 1, 1, 10].into()
        })
    );

    // with label.
    let mut parser = parse_text("continue outerLoop;");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{:?}", err)),
        Statement::ContinueStatement(ContinueStatement {
            label: Some(Identifier {
                name: format!("outerLoop"),
                span: [1, 10, 1, 19].into()
            }),
            span: [1, 1, 1, 20].into()
        })
    );
}

#[test]
fn parse_break_statement() {
    let mut parser = parse_text("break;");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{:?}", err)),
        Statement::BreakStatement(BreakStatement {
            label: None,
            span: [1, 1, 1, 7].into()
        })
    );

    // with label.
    let mut parser = parse_text("break outerLoop;");
    parser.debug_allow_global_expressions = true;
    assert_eq!(
        parser.next().unwrap().expect(|err| format!("{:?}", err)),
        Statement::BreakStatement(BreakStatement {
            label: Some(Identifier {
                name: format!("outerLoop"),
                span: [1, 7, 1, 16].into()
            }),
            span: [1, 1, 1, 17].into()
        })
    );
}

// #[test]
// fn parse_expression_statement() {
//     let mut parser = parse_text(
//         "public function main() {
//     app := new Server(\"main-app\", 8080);

//     app.Get(\"/user\", CreateUser)!;

//     // app.Start().Then(fn() {
//     //     core.io.Printf(\"Server is listening on port %d\" + app.port);
//     // }).Run().Await()!;

//     app.Start();
// }",
//     );
//     parser.debug_allow_global_expressions = true;
//     assert_eq!(
//         parser.next().unwrap().expect(|err| format!("{err:?}")),
//         Statement::ExpressionStatement(Expression::NumberLiteral(WhirlNumber {
//             value: ast::Number::Decimal(format!("1")),
//             span: [1, 1, 1, 1].into()
//         }))
//     )
// }
