use crate::lex::TokenStream;

mod block;
mod comment;
mod errors;
mod expr;
mod module;
mod stmt;

pub use block::*;
pub use comment::*;
pub use errors::{ParseError, ParseResult, SyntaxError};
pub use expr::*;
pub use module::*;
pub use stmt::*;

/// Parsing interface for any syntax tree node.
pub trait Parse: Sized {
    fn parse(input: &mut TokenStream) -> ParseResult<Self>;
}

// TODO: Parse:parse() should take this context instead of TokenStream.

/// Contextual information for the recursive parser.
pub struct ParseContext<'a> {
    pub tokens: TokenStream<'a>,
    /// Stack of classes being parsed.
    pub classes: Vec<ClassInfo>,
}

/// Bookkeeping information for compiling a class.
pub struct ClassInfo {
    /// Indicates whether the current class is foreign.
    /// Fields cannot be defined in foreign classes.
    pub is_foreign: bool,
    /// Current method being compiled.
    pub method: Option<MethodInfo>,
}

pub struct MethodInfo {
    /// Indicates whether the current method being compiled is static.
    /// Instance fields cannot be used from static methods.
    pub in_static: bool,
}
