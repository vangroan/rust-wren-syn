use std::{error::Error, fmt};

/// Error returned when parsing phase fails.
#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
    // TODO: Span
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error: {}", self.msg)
    }
}
