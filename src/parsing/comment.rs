//! Comment parsing.
use super::{
    errors::{ParseResult, SyntaxError},
    Parse,
};
use crate::{lex::TokenStream, token::TokenType};

#[derive(Debug)]
pub struct Comment {
    pub text: String,
}

impl Parse for Comment {
    fn parse(input: &mut TokenStream) -> ParseResult<Self> {
        use TokenType as T;

        if input.match_token(T::CommentLeft) {
            // Block comment
            todo!("block comments not implemented yet");
        } else {
            // Line comment
            let token = input.consume(T::CommentLine)?;
            let literal = token
                .lit
                .as_ref()
                .and_then(|lit| lit.comment())
                .ok_or_else(|| SyntaxError {
                    msg: "comment token has no literal".to_string(),
                })?;

            let span = token.span;

            // TODO: Use fragments for literals to avoid string copies.
            println!(
                "Fragment: {}",
                input.fragment(span.start..(span.start + span.count)).unwrap()
            );

            Ok(Comment {
                text: literal.to_string(),
            })
        }
    }
}

impl Comment {
    /// Parsing utility to consume new lines and
    /// comments.
    ///
    /// Since comments are not discarded during
    /// tokenisation, they end up as tokens in the parser
    /// and generally get in the way.
    ///
    /// Comments should be inserted into the generated
    /// abstract-syntax-tree, but it's not exactly clear
    /// how to handle each parsing case.
    ///
    /// Until the day that all comment cases can be handled,
    /// we need to ignore comments during parsing.
    pub fn ignore(input: &mut TokenStream) {
        use TokenType as T;

        input.reset_peek();

        loop {
            if input.match_token(T::CommentLine)
                || input.match_token(T::Comment)
                || input.match_token(T::CommentLeft)
                || input.match_token(T::CommentRight)
            {
                continue;
            } else {
                break;
            }
        }
    }
}
