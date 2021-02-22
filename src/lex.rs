use itertools::{multipeek, MultiPeek};
use smol_str::SmolStr;
use std::str::CharIndices;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    Dot,

    Ident,
    Keyword(KeywordType),

    Number,
    String,

    Newline,
    EOF,
}

#[derive(Debug, PartialEq, Eq)]
pub enum KeywordType {
    False,
    True,
}

/// Literal value.
#[derive(Debug)]
pub enum Lit {
    Number(f64),
    String(String),
}

#[derive(Debug)]
pub struct Ident {
    pub name: SmolStr,
}

#[derive(Debug)]
pub struct Token {
    pub ty: TokenType,
    pub lit: Option<Lit>,
    pub ident: Option<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Span {
    /// Starting byte position.
    pub start: usize,
    /// Number of bytes covered by the span.
    pub count: usize,
    /// Column position in text where the span starts, starting at 1.
    pub column: usize,
    /// Line position in text where the span starts, starting at 1.
    pub line: usize,
}

pub struct Lexer<'a> {
    source: MultiPeek<CharIndices<'a>>,

    /// Starting position of the token that is currently being parsed.
    token_start: usize,

    /// Starting column of the current token being lexed. Starts at 1.
    column_start: usize,

    /// Position in the source string of the current character.
    current: (usize, char),

    /// Current column of the current character being lexed. Starts at 1.
    current_column: usize,

    /// Current line of the current character being lexed. Starts at 1.
    current_line: usize,

    /// Temporary buffer space for collecting token characters while consuming.
    buf: String,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer` for the given string.
    ///
    /// Returns `None` when the string is empty.
    pub fn new(source: &'a str) -> Self {
        Self {
            source: multipeek(source.char_indices()),
            token_start: 0,
            column_start: 1,
            current: (0, '\0'),
            current_column: 1,
            current_line: 1,
            buf: String::new(),
        }
    }

    /// Consumes the `Lexer` and returns tokens parsed from the borrowed string.
    pub fn into_tokens(mut self) -> Vec<Token> {
        let mut tokens = vec![];
        while let Some(token) = self.next_token() {
            tokens.push(token);
        }
        tokens
    }

    pub fn next_token(&mut self) -> Option<Token> {
        while self.peek_char().is_some() {
            self.start_token();

            if let Some((_, c)) = self.next_char() {
                match c {
                    '(' => {
                        return Some(self.make_token(TokenType::LeftParen));
                    }
                    ')' => {
                        return Some(self.make_token(TokenType::RightParen));
                    }
                    '.' => {
                        return Some(self.make_token(TokenType::Dot));
                    }
                    '"' => return self.consume_string(TokenType::String),
                    '\n' => {
                        return Some(self.make_token(TokenType::Newline));
                    }
                    ' ' | '\t' | '\r' => {
                        self.consume_whitespace();
                    }
                    _ => {
                        if Self::is_ident(c) {
                            return Some(self.consume_ident(TokenType::Ident));
                        } else {
                            // TODO: Error
                        }
                    }
                }
            }
        }

        None
    }

    fn next_char(&mut self) -> Option<(usize, char)> {
        if let Some((index, c)) = self.source.next() {
            if c == '\n' {
                self.current_column += 1;
                self.current_line += 1;
            } else {
                self.current_column += 1;
            }
            self.current = (index, c);
            Some((index, c))
        } else {
            None
        }
    }

    #[inline]
    fn peek_char(&mut self) -> Option<(usize, char)> {
        self.source.peek().cloned()
    }

    fn start_token(&mut self) {
        self.token_start = self.current.0;
        self.column_start = self.current_column;
    }

    fn make_token(&mut self, token_ty: TokenType) -> Token {
        let ident = if token_ty == TokenType::Ident {
            // Identifier name was consumed and stored in the buffer.
            let name = self.drain_buffer();
            Some(Ident { name })
        } else {
            None
        };

        // String literal
        let lit = if token_ty == TokenType::String {
            Some(Lit::String(self.buf.clone()))
        } else {
            None
        };

        // Build span.
        let span = Span {
            start: self.token_start,
            count: self.current.0 - self.token_start,
            column: self.column_start,
            line: self.current_line,
        };

        Token {
            ty: token_ty,
            lit,
            ident,
            span,
        }
    }

    /// Checks whether the given character is considered valid for
    /// an identifier name.
    fn is_ident(c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    }

    fn is_number(c: char) -> bool {
        c >= '0' && c <= '9'
    }

    fn consume_ident(&mut self, token_ty: TokenType) -> Token {
        self.start_buffer();

        while let Some((_, c)) = self.peek_char() {
            if Self::is_ident(c) || Self::is_number(c) {
                self.buf.push(c);
                self.next_char();
            } else {
                break;
            }
        }

        // TODO: If the buffer contains a known keyword, change token type to Keyword
        self.make_token(token_ty)
    }

    fn consume_string(&mut self, token_ty: TokenType) -> Option<Token> {
        // Exclude the first quote character from literal.
        self.buf.clear();

        loop {
            if let Some((_, c)) = self.peek_char() {
                if c == '"' {
                    // Don't push the last quote
                    self.next_char();
                    break;
                }
                self.buf.push(c);
                self.next_char();
            // TODO: String escaping
            } else {
                // Unterminated string
                return None;
            }
        }

        Some(self.make_token(token_ty))
    }

    fn consume_whitespace(&mut self) {
        while let Some((_, ' ')) | Some((_, '\t')) | Some((_, '\r')) = self.peek_char() {
            self.next_char();
        }
    }

    fn start_buffer(&mut self) {
        self.buf.clear();
        self.buf.push(self.current.1);
    }

    fn drain_buffer(&mut self) -> SmolStr {
        let s = SmolStr::new(&self.buf);
        self.buf.clear();
        s
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lexer_is_ident() {
        assert!(Lexer::is_ident('_'));
        assert!(Lexer::is_ident('a'));
        assert!(Lexer::is_ident('b'));
        assert!(Lexer::is_ident('y'));
        assert!(Lexer::is_ident('z'));
        assert!(Lexer::is_ident('A'));
        assert!(Lexer::is_ident('B'));
        assert!(Lexer::is_ident('Y'));
        assert!(Lexer::is_ident('Z'));

        assert!(!Lexer::is_ident('0'));
        assert!(!Lexer::is_ident('1'));
        assert!(!Lexer::is_ident('8'));
        assert!(!Lexer::is_ident('9'));
        assert!(!Lexer::is_ident('-'));
        assert!(!Lexer::is_ident('/'));
        assert!(!Lexer::is_ident('?'));
        assert!(!Lexer::is_ident(' '));
    }

    #[test]
    fn test_lexer_next_char() {
        let mut lexer = Lexer::new("System.print(\"Hello, world!\")");
        let chars = [(0, 'S'), (1, 'y'), (2, 's'), (3, 't'), (4, 'e'), (5, 'm'), (6, '.')];
        for (index, c) in &chars {
            assert_eq!(lexer.next_char(), Some((*index, *c)));
        }
    }

    #[test]
    fn test_lexer_next_token() {
        let mut lexer = Lexer::new("System.print(\"Hello, world!\")");

        println!("{:#?}", lexer.next_token());
        println!("{:#?}", lexer.next_token());
        println!("{:#?}", lexer.next_token());
        println!("{:#?}", lexer.next_token());
        println!("{:#?}", lexer.next_token());
        println!("{:#?}", lexer.next_token());
        // assert_eq!(lexer.next_token().map(|t| t.ty), Some(TokenType::Ident));
        // assert_eq!(lexer.next_token().map(|t| t.ty), Some(TokenType::Dot));
        // assert_eq!(lexer.next_token().map(|t| t.ty), Some(TokenType::Ident));
        // assert_eq!(lexer.next_token().map(|t| t.ty), Some(TokenType::LeftParen));
        // assert_eq!(lexer.next_token().map(|t| t.ty), Some(TokenType::String));
        // assert_eq!(
        //     lexer.next_token().map(|t| t.ty),
        //     Some(TokenType::RightParen)
        // );
    }

    #[test]
    fn test_lex_from_string() {
        let source = String::from("System.print(\"Hello, world!\")");
        let lexer = Lexer::new(&source);
        assert_eq!(lexer.into_tokens().len(), 6);
    }
}
