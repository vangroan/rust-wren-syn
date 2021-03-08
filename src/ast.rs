use crate::token::{Ident, Token};

#[derive(Debug)]
pub enum Syntax {
    Stmts(Vec<Stmt>),
    Stmt(Stmt),
    Expr(Expr),
}

impl Syntax {
    pub fn expr(&self) -> Option<&Expr> {
        match self {
            Syntax::Expr(expr) => Some(expr),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    /// Definition statement.
    Def(Def),
    /// Expression statement.
    Expr,
}

/// Definition statement.
#[derive(Debug)]
pub enum Def {
    Class(ClassDef),
}

#[derive(Debug)]
pub struct ClassDef {
    /// Class name.
    pub ident: Ident,
    /// Base class that this class is inheriting from.
    pub parent: Option<Ident>,
    /// TODO: Member methods.
    pub members: ClassMembers,
}

/// Class-level statements.
#[derive(Debug, Default)]
pub struct ClassMembers {
    pub construct: Option<()>,
    pub fields: Vec<()>,
    pub props: Vec<()>,
    pub methods: Vec<()>,
    pub ops: Vec<()>,
}

#[derive(Debug)]
pub enum Expr {
    Num(NumLit),
    UnOp(UnaryOp),
    BinOp(BinaryOp),
}

impl Expr {
    pub fn number(&self) -> Option<&NumLit> {
        match self {
            Expr::Num(number) => Some(number),
            _ => None,
        }
    }

    pub fn binary(&self) -> Option<&BinaryOp> {
        match self {
            Expr::BinOp(binary) => Some(binary),
            _ => None,
        }
    }
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
    pub operator: Token,
    pub rhs: Box<Expr>,
}

/// Arithmetic operation with an expression on either side.
#[derive(Debug)]
pub struct BinaryOp {
    pub operator: Token,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}
