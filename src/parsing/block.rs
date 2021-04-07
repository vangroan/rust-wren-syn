//! Functions and anonymous blocks (stub functions).
use super::{expr::Expr, stmt::DefStmt, Parse, ParseResult};
use crate::lex::TokenStream;

pub struct Block {
    /// Blocks can have zero or one argument.
    _arg: Option<()>,
    /// Any definition statment is valid in a function body.
    _body: BlockBody,
}

/// Body of a function or block.
///
/// A body can be a single expression, where implicitly the result of the expression
/// is the return of the block.
///
/// It can also be list of statements.
#[derive(Debug)]
pub enum BlockBody {
    Expr(Expr),
    Stmts(Vec<DefStmt>),
}

impl Parse for BlockBody {
    fn parse(_input: &mut TokenStream) -> ParseResult<Self> {
        todo!()
    }
}
