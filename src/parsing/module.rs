//! Wren module parsing.
use super::{stmt::DefStmt, Parse, ParseResult};
use crate::{lex::TokenStream, token::TokenType};

/// Wren script file.
#[derive(Debug)]
pub struct Module {
    stmts: Vec<DefStmt>,
}

impl Parse for Module {
    fn parse(input: &mut TokenStream) -> ParseResult<Self> {
        println!("Module::parse");
        use TokenType as T;

        input.reset_peek();
        let mut stmts = vec![];

        while let Some(token) = input.peek() {
            match token.ty {
                T::Newline => {
                    // When the statement starts with a newline, it's blank.
                    input.next_token();
                    continue;
                }
                T::EOF => break,
                _ => stmts.push(DefStmt::parse(input)?),
            }
        }

        Ok(Self { stmts })
    }
}
