use crate::lex::Token;

#[derive(Debug)]
pub enum Syntax {
    Stmts(Vec<Stmt>),
    Expr(Expr),
}

#[derive(Debug)]
pub struct Stmt {}

#[derive(Debug)]
pub enum Expr {
    Num(NumLit),
    UnOp(UnaryOp),
    BinOp(BinaryOp),
}

/// Number literal.
#[derive(Debug)]
pub struct NumLit {
    pub token: Token,
    pub notation: Notation,
}

/// Format in which the numeral was written.
#[derive(Debug)]
pub enum Notation {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
    Scientific,
}

/// Arithmetic operation with an expression on the right side.
#[derive(Debug)]
pub struct UnaryOp {
    pub operand: Token,
    pub rhs: Box<Expr>,
}

/// Arithmetic operation with an expression on either side.
#[derive(Debug)]
pub struct BinaryOp {
    pub operand: Token,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}
