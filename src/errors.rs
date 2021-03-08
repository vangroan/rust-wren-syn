use std::{error::Error, fmt};

/// Error returned when parsing phase fails.
#[derive(Debug)]
pub struct SyntaxError {
    pub msg: String,
    // TODO: Span
}

impl Error for SyntaxError {}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error: {}", self.msg)
    }
}
