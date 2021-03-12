use crate::lex::TokenStream;

mod comment;
mod errors;
mod expr;
mod module;
mod stmt;

pub use comment::*;
pub use errors::{ParseError, ParseResult, SyntaxError};
pub use expr::*;
pub use module::*;
pub use stmt::*;

/// Parsing interface for any syntax tree node.
pub trait Parse: Sized {
    fn parse(input: &mut TokenStream) -> ParseResult<Self>;
}
