//! Test class definition parsing.

use rust_wren_syn::{DefStmt, Lexer, Module, Parse, TokenStream};

#[test]
fn test_empty_class() {
    let lexer = Lexer::new("class Foo {}");
    let mut tokens = TokenStream::new(lexer);

    let cls_def = DefStmt::parse(&mut tokens).unwrap();
    println!("{:#?}", cls_def);
}

#[test]
fn test_derived_empty_class() {
    let lexer = Lexer::new("class Foo is Bar {}");
    let mut tokens = TokenStream::new(lexer);

    let cls_def = DefStmt::parse(&mut tokens).unwrap();
    println!("{:#?}", cls_def);
}

#[test]
fn test_derived_empty_class_stmt() {
    let lexer = Lexer::new("class Foo is Bar {}");
    let mut tokens = TokenStream::new(lexer);

    let cls_def = DefStmt::parse(&mut tokens).unwrap();
    println!("{:#?}", cls_def);
}

#[test]
fn test_class_construct() {
    let lexer = Lexer::new(
        r#"class Foo {
        construct new() {}
    }"#,
    );
    let mut tokens = TokenStream::new(lexer);

    let cls_def = DefStmt::parse(&mut tokens);
    println!("{:#?}", cls_def);
}

/// Comment allowed after class definition.
#[test]
fn test_class_def_comment() {
    let lexer = Lexer::new("class Foo {} // annotate");
    let mut tokens = TokenStream::new(lexer);

    let cls_def = DefStmt::parse(&mut tokens).unwrap();
    println!("{:#?}", cls_def);
}

#[test]
fn test_class_docstring() {
    let lexer = Lexer::new(
        r#"
    // Docstring
    //
    // # Examples
    //
    // ```
    // Foo.new()
    // ```
    class Foo {
        // Creates a `Foo` using `value`.
        // Second line
        // Third line
        construct new(value) {
            // Comment in function
            _value = value

            var foobar = value + _value + 2
        }
    }
    "#,
    );
    let mut tokens = TokenStream::new(lexer);

    let body = Module::parse(&mut tokens).unwrap();
    println!("{:#?}", body);
}
