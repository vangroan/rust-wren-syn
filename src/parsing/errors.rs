use crate::lex::TokenError;
use std::{error::Error, fmt};

pub type ParseResult<T> = Result<T, ParseError>;

/// Error returned when parsing phase fails.
#[derive(Debug)]
pub enum ParseError {
    Token(TokenError),
    Syntax(SyntaxError),
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ParseError::Token(err) => Some(err),
            ParseError::Syntax(err) => Some(err),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::Token(err) => write!(f, "token error: {}", err),
            ParseError::Syntax(err) => write!(f, "syntax error: {}", err),
        }
    }
}

impl From<SyntaxError> for ParseError {
    fn from(syn_err: SyntaxError) -> Self {
        Self::Syntax(syn_err)
    }
}

impl From<TokenError> for ParseError {
    fn from(token_err: TokenError) -> Self {
        Self::Token(token_err)
    }
}

/// Error returned when parsing phase fails.
#[derive(Debug)]
pub struct SyntaxError {
    pub msg: String,
    // TODO: Span
}

impl Error for SyntaxError {}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}
