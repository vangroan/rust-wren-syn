//! Token types outputted by lexing.

use crate::Span;
use smol_str::SmolStr;
use std::{convert::TryFrom, fmt};

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

impl fmt::Display for TokenType {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenType as T;

        match self {
            T::LeftParen        => write!(f, "("),
            T::RightParen       => write!(f, ")"),
            T::LeftBracket      => write!(f, "["),
            T::RightBracket     => write!(f, "]"),
            T::LeftBrace        => write!(f, "{{"),
            T::RightBrace       => write!(f, "}}"),
            T::Dot              => write!(f, "."),
            T::Add              => write!(f, "+"),
            T::Sub              => write!(f, "-"),
            T::Mul              => write!(f, "*"),
            T::Div              => write!(f, "/"),
            T::Ident            => write!(f, "identifier"),
            T::Keyword(keyword) => fmt::Display::fmt(keyword, f),
            T::Number           => write!(f, "number"),
            T::String           => write!(f, "string"),
            T::Interpolated     => write!(f, "interpolated"),
            T::CommentLine      => write!(f, "//"),
            T::CommentLeft      => write!(f, "/*"),
            T::CommentRight     => write!(f, "*/"),
            T::Comment          => write!(f, "comment"),
            T::Newline          => write!(f, "//n"),
            T::EOF              => write!(f, "end-of-file"),
        }
    }
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

    #[rustfmt::skip]
    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        use KeywordType as K;
        match value {
            "break"      => Ok(K::Break),
            "class"      => Ok(K::Class),
            "construct"  => Ok(K::Construct),
            "continue"   => Ok(K::Continue),
            "false"      => Ok(K::False),
            "for"        => Ok(K::For),
            "foreign"    => Ok(K::Foreign),
            "if"         => Ok(K::If),
            "import"     => Ok(K::Import),
            "is"         => Ok(K::Is),
            "return"     => Ok(K::Return),
            "static"     => Ok(K::Static),
            "super"      => Ok(K::Super),
            "this"       => Ok(K::This),
            "true"       => Ok(K::True),
            "var"        => Ok(K::Var),
            "while"      => Ok(K::While),
            _ => Err(()),
        }
    }
}

impl fmt::Display for KeywordType {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use KeywordType as K;
        match self {
            K::Break        => write!(f, "break"),
            K::Class        => write!(f, "class"),
            K::Construct    => write!(f, "construct"),
            K::Continue     => write!(f, "continue"),
            K::False        => write!(f, "false"),
            K::For          => write!(f, "for"),
            K::Foreign      => write!(f, "foreign"),
            K::If           => write!(f, "if"),
            K::Import       => write!(f, "import"),
            K::Is           => write!(f, "is"),
            K::Return       => write!(f, "return"),
            K::Static       => write!(f, "static"),
            K::Super        => write!(f, "super"),
            K::This         => write!(f, "this"),
            K::True         => write!(f, "true"),
            K::Var          => write!(f, "var"),
            K::While        => write!(f, "while"),
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

impl Lit {
    #[inline]
    pub fn number(&self) -> Option<&str> {
        match self {
            Lit::Number(s) => Some(s),
            _ => None,
        }
    }
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
