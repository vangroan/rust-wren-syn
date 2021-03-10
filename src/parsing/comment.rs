//! Comment parsing.
use crate::{lex::TokenStream, token::TokenType};

#[derive(Debug)]
pub struct Comment {
    pub text: String,
}

impl Comment {
    /// Parsing utility to consume new lines and
    /// comments.
    ///
    /// Since comments are not discarded during
    /// tokenisation, they end up as tokens in the parser
    /// and generally get ni the way.
    ///
    /// Comments should be inserted into the generated
    /// abstract-syntax-tree, but it's not exactly clear
    /// how to handle each parsin case.
    ///
    /// Until the day that all comment cases can be handled,
    /// we need to ignore comments during parsing.
    pub fn ignore(input: &mut TokenStream) {
        use TokenType as T;

        input.reset_peek();

        loop {
            input.match_lines();

            if let Some(token_ty) = input.peek().map(|t| t.ty) {
                if let T::CommentLine | T::Comment | T::CommentLeft | T::CommentRight = token_ty {
                    input.next_token();
                } else {
                    // Encountered something other than a comment.
                    break;
                }
            } else {
                // EOF
                break;
            }
        }
    }
}
