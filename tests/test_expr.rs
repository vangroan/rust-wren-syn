//! Test expression parsing.

use rust_wren_syn::{Lexer, Parser, TokenType};

/// Simple 1 + 2 * 3 test case.
#[test]
fn test_simple() {
    let lexer = Lexer::new("1 + 2 * 3");
    let tokens = lexer.into_tokens();
    println!("{:?}", tokens);

    let parser = Parser::new(tokens);
    let ast = parser.parse_script();
    println!("{:#?}", ast);

    let add = ast.first().unwrap().expr().unwrap().binary().unwrap();
    assert_eq!(add.operator.ty, TokenType::Add);

    let one = add.lhs.number().unwrap();
    assert_eq!(one.token.ty, TokenType::Number);

    let mul = add.rhs.binary().unwrap();
    assert_eq!(mul.operator.ty, TokenType::Mul);

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
    let lexer = Lexer::new("1 * 2 + 3");
    let tokens = lexer.into_tokens();
    println!("{:?}", tokens);

    let parser = Parser::new(tokens);
    let ast = parser.parse_script();
    println!("{:#?}", ast);

    let add = ast.first().unwrap().expr().unwrap().binary().unwrap();
    assert_eq!(add.operator.ty, TokenType::Add);

    let mul = add.lhs.binary().unwrap();
    assert_eq!(mul.operator.ty, TokenType::Mul);

    let one = mul.lhs.number().unwrap();
    assert_eq!(one.token.ty, TokenType::Number);

    let two = mul.rhs.number().unwrap();
    assert_eq!(two.token.ty, TokenType::Number);

    let three = add.rhs.number().unwrap();
    assert_eq!(three.token.ty, TokenType::Number);
}

/// Simple `1 + 2 - 3` test case.
///
/// The operators `+` and `-` have the same precedence,
/// and left-associativity. The operand in the middle,
/// `_ + 2 - _` is bound to the left operator tighter
/// than the right.
///
/// Expected grouping of syntax nodes is `(1 + 2) - 3`.
///
/// If the result is `1 + (2 - 3)` then the operators
/// were incorrectly parsed with right-associativity.
#[test]
fn test_associativity() {
    let lexer = Lexer::new("1 + 2 - 3");
    let tokens = lexer.into_tokens();
    println!("{:?}", tokens);

    let parser = Parser::new(tokens);
    let ast = parser.parse_script();
    println!("{:#?}", ast);
}
