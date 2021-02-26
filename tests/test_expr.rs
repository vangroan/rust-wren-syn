//! Test expression parsing.

use rust_wren_syn::{BinaryOp, Expr, Lexer, Parser, Syntax, TokenType};

/// Simple 1 + 2 * 3 test case.
#[test]
fn test_simple() {
    let mut lexer = Lexer::new("1 + 2 * 3");
    let tokens = lexer.into_tokens();
    println!("{:?}", tokens);

    let mut parser = Parser::new(tokens);
    let ast = parser.parse_script();
    println!("{:#?}", ast);

    let add = ast.first().unwrap().expr().unwrap().binary().unwrap();
    assert_eq!(add.operand.ty, TokenType::Add);

    let one = add.lhs.number().unwrap();
    assert_eq!(one.token.ty, TokenType::Number);

    let mul = add.rhs.binary().unwrap();
    assert_eq!(mul.operand.ty, TokenType::Mul);

    let two = mul.lhs.number().unwrap();
    assert_eq!(two.token.ty, TokenType::Number);

    let three = mul.rhs.number().unwrap();
    assert_eq!(three.token.ty, TokenType::Number);
}

/// Simple 1 * 2 + 3 test case.
///
/// Expected parsing output is the have the Add
/// binary operator at the root of the syntax tree.
///
/// If the multiply is the root, the precedence is
/// not working correctly.
#[test]
fn test_precedence() {
    let mut lexer = Lexer::new("1 * 2 + 3");
    let tokens = lexer.into_tokens();
    println!("{:?}", tokens);

    let mut parser = Parser::new(tokens);
    let ast = parser.parse_script();
    println!("{:#?}", ast);

    let add = ast.first().unwrap().expr().unwrap().binary().unwrap();
    assert_eq!(add.operand.ty, TokenType::Add);

    let mul = add.lhs.binary().unwrap();
    assert_eq!(mul.operand.ty, TokenType::Mul);

    let one = mul.lhs.number().unwrap();
    assert_eq!(one.token.ty, TokenType::Number);

    let two = mul.rhs.number().unwrap();
    assert_eq!(two.token.ty, TokenType::Number);

    let three = add.rhs.number().unwrap();
    assert_eq!(three.token.ty, TokenType::Number);
}
