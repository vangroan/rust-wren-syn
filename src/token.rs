//! Token types outputted by lexing.

use std::convert::TryFrom;
use std::fmt;

use smol_str::SmolStr;

use crate::Span;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    LeftBrace,    // {
    RightBrace,   // }
    Dot,
    Add,
    Sub,
    Mul,
    Div,

    Ident,
    Keyword(KeywordType),

    Number,
    String,
    Interpolated,

    /// Code comment that starts with `//`
    CommentLine,
    /// Opening comment `/*`
    CommentLeft,
    /// Closing comment `*/`
    CommentRight,
    /// Contents of a comment block, between the
    /// `/*` and `*/`.
    Comment,

    Newline,
    EOF,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum KeywordType {
    Break,
    Class,
    Construct,
    Continue,
    False,
    For,
    Foreign,
    If,
    Import,
    Is,
    Return,
    Static,
    Super,
    This,
    True,
    Var,
    While,
}

// TODO: Replace with from_str
impl<'a> TryFrom<&'a str> for KeywordType {
    type Error = ();

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        match value {
            "break" => Ok(KeywordType::Break),
            "class" => Ok(KeywordType::Class),
            "construct" => Ok(KeywordType::Construct),
            "continue" => Ok(KeywordType::Continue),
            "false" => Ok(KeywordType::False),
            "for" => Ok(KeywordType::For),
            "foreign" => Ok(KeywordType::Foreign),
            "if" => Ok(KeywordType::If),
            "import" => Ok(KeywordType::Import),
            "is" => Ok(KeywordType::Is),
            "return" => Ok(KeywordType::Return),
            "static" => Ok(KeywordType::Static),
            "super" => Ok(KeywordType::Super),
            "this" => Ok(KeywordType::This),
            "true" => Ok(KeywordType::True),
            "var" => Ok(KeywordType::Var),
            "while" => Ok(KeywordType::While),
            _ => Err(()),
        }
    }
}

/// Literal value.
#[derive(Debug, Clone)]
pub enum Lit {
    Number(String),
    String(String),
    Comment(String),
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lit::Number(s) => fmt::Display::fmt(s.as_str(), f),
            Lit::String(s) => fmt::Display::fmt(s.as_str(), f),
            Lit::Comment(s) => fmt::Display::fmt(s.as_str(), f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: SmolStr,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub ty: TokenType,
    pub lit: Option<Lit>,
    pub ident: Option<Ident>,
    pub span: Span,
}
