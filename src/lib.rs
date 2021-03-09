#[deprecated]
pub mod ast;
mod lex;
mod parsing;
mod token;

pub use lex::*;
pub use parsing::*;
pub use token::*;

/// Parse the given string as Wren source.
///
/// ```
/// use rust_wren_syn::{parse, DefStmt};
///
/// const SOURCE: &str = r#"class Foo is Bar {}"#;
///
/// let statement = parse::<DefStmt>(SOURCE).expect("Parsing failed");
/// ```
pub fn parse<T>(source: &str) -> ParseResult<T>
where
    T: Parse,
{
    let lexer = Lexer::new(source);
    let mut tokens = TokenStream::new(lexer);
    T::parse(&mut tokens)
}
