//! Test class definition parsing.

use rust_wren_syn::{Lexer, Parser, TokenStream, DefStmt, Parse};

#[test]
fn test_empty_class() {
    let lexer = Lexer::new("class Foo {}");
    let tokens = lexer.into_tokens();
    println!("{:#?}", tokens);

    let parser = Parser::new(tokens);
    let ast = parser.parse_script();
    println!("{:#?}", ast);
}

#[test]
fn test_derived_empty_class() {
    let lexer = Lexer::new("class Foo is Bar {}");
    let tokens = lexer.into_tokens();
    println!("{:#?}", tokens);

    let parser = Parser::new(tokens);
    let ast = parser.parse_script();
    println!("{:#?}", ast);
}

#[test]
fn test_derived_empty_class_stmt() {
    let lexer = Lexer::new("class Foo is Bar {}");
    let mut tokens = TokenStream::new(lexer);

    let stmt = DefStmt::parse(&mut tokens).unwrap();
    println!("{:#?}", stmt);
}

#[test]
fn test_class_construct() {
    // let lexer = Lexer::new(r#"class Foo {
    //     construct new() {}
    // }"#);
    // let tokens = lexer.into_tokens();
    // println!("{:#?}", tokens);

    // let parser = Parser::new(tokens);
    // let ast = parser.parse_script();
    // println!("{:#?}", ast);
}
