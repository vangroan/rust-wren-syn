//! Test expression parsing.

use rust_wren_syn::{Lexer, Parser};

/// Simple 1 + 2 * 3 test case.
#[test]
fn test_simple() {
    let mut lexer = Lexer::new("1 + 2 * 3");
    let tokens = lexer.into_tokens();
    println!("{:?}", tokens);

    let mut parser = Parser::new(tokens);
    let ast = parser.parse_script();
    println!("{:#?}", ast);
}
