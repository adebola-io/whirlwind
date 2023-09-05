#![cfg(test)]

use whirl_ast::{
    Block, FunctionDeclaration, ScopeAddress, ScopeEntry, Span, Statement, TypeExpression,
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
