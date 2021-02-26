use itertools::{multipeek, MultiPeek};
use smol_str::SmolStr;
use std::str::CharIndices;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    LeftParen,
    RightParen,
    Dot,
    Add,
    Sub,
    Mul,
    Div,

    Ident,
    Keyword(KeywordType),

    Number,
    String,

    Newline,
    EOF,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum KeywordType {
    False,
    True,
}

/// Literal value.
#[derive(Debug, Clone)]
pub enum Lit {
    Number(String),
    String(String),
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
    /// Iterator over UTF-8 encoded source code.
    ///
    /// The `MultiPeek` wrapper allows for arbitrary lookahead by consuming
    /// the iterator internally and buffering the result. This is required
    /// because UTF-8 characters are variable in width. Indexing the string
    /// for individual bytes is possible, but impossible for encoded characters.
    ///
    /// An important semantic feature of `MultiPeek` is that peeking advances
    /// the internal peek cursor by 1. Each call will return the next element.
    /// The peek cursor offset is restored to 0 when calling `MultiPeek::next()`
    /// or `MultiPeek::reset_peek()`.
    source: MultiPeek<CharIndices<'a>>,

    /// Number of bytes, not characters or graphemes, in the source string.
    source_size: usize,

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
    pub fn from_str(source: &'a str) -> Self {
        Self {
            source: multipeek(source.char_indices()),
            source_size: source.len(),
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
        while !self.at_end() {
            println!("peeked");
            println!("current: {:?}", self.current);
            self.start_token();

            if let Some((index, c)) = self.next_char() {
                println!("next_token: ({}, '{}')", index, c);

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
                    '+' => return Some(self.make_token(TokenType::Add)),
                    '-' => return Some(self.make_token(TokenType::Sub)),
                    '*' => return Some(self.make_token(TokenType::Mul)),
                    '/' => return Some(self.make_token(TokenType::Div)),
                    '\n' => {
                        return Some(self.make_token(TokenType::Newline));
                    }
                    ' ' | '\t' | '\r' => {
                        self.consume_whitespace();
                    }
                    _ => {
                        if Self::is_digit(c) {
                            return Some(self.consume_number(TokenType::Number));
                        } else if Self::is_ident(c) {
                            return Some(self.consume_ident(TokenType::Ident));
                        } else {
                            // TODO: Error
                            // error: unknown start of token: {}
                            panic!("error: unknown character");
                        }
                    }
                }
            } else {
                self.current.0 = self.source_size + 1;
                return Some(self.make_token(TokenType::EOF));
            }
        }

        None
    }

    fn next_char(&mut self) -> Option<(usize, char)> {
        if let Some((index, c)) = self.source.next() {
            println!("next_char ({}, '{}')", index, c);

            if c == '\n' {
                self.current_column += 1;
                self.current_line += 1;
            } else {
                self.current_column += 1;
            }
            self.current = (index, c);
            Some((index, c))
        } else {
            // Source code iterator has reached end-of-file.
            //
            // Set the current index to the size of the source
            // string. There is no End-of-file character, so
            // we just set it to the null-byte.
            self.current = (self.source_size, '\0');
            None
        }
    }

    /// Peeks the current character in the stream.
    ///
    /// This call advances the peek cursor. Subsequent
    /// calls will look ahead by one character each call.
    #[inline]
    fn peek_char(&mut self) -> Option<(usize, char)> {
        self.source.peek().cloned()
    }

    /// Reset the stream peek cursor.
    fn reset_peek(&mut self) {
        self.source.reset_peek()
    }

    fn start_token(&mut self) {
        self.token_start = self.current.0;
        self.column_start = self.current_column;
    }

    fn make_token(&mut self, token_ty: TokenType) -> Token {
        println!("make_token: {:?} {:?}", self.current, token_ty);

        let ident = if token_ty == TokenType::Ident {
            // Identifier name was consumed and stored in the buffer.
            let name = self.drain_buffer();
            Some(Ident { name })
        } else {
            None
        };

        // Literals
        let lit = match token_ty {
            TokenType::String => Some(Lit::String(self.buf.clone())),
            TokenType::Number => Some(Lit::Number(self.buf.clone())),
            _ => None,
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

    fn is_digit(c: char) -> bool {
        c >= '0' && c <= '9'
    }

    fn consume_ident(&mut self, token_ty: TokenType) -> Token {
        self.start_buffer();

        while let Some((_, c)) = self.peek_char() {
            if Self::is_ident(c) || Self::is_digit(c) {
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

        // Ensure clean peek state.
        self.reset_peek();

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

    fn consume_number(&mut self, token_ty: TokenType) -> Token {
        self.start_buffer();

        // Ensure clean peek state.
        self.reset_peek();

        // Consume tokens of the number literal until we
        // encounter a character that's invalid for any
        // of the supported numeral notations.
        loop {
            match self.peek_char() {
                Some((_, c)) => {
                    // TODO: Support numeral notations like binary '0b...', hex '0x..', etc.
                    if Self::is_digit(c) {
                        // Check is performed on peeked char
                        // because we don't want to consume
                        // something that terminates the numeral,
                        // eg. whitespace.
                        self.next_char();
                    } else {
                        break;
                    }
                }
                None => {
                    // Reached end of file.
                    break;
                }
            }
        }

        self.make_token(token_ty)
    }

    /// Consumes whitespace, excluding new lines, until a non-whitespace
    /// character is encountered.
    ///
    /// New lines are statement terminator tokens in Wren, and shouldn't
    /// be ignored.
    fn consume_whitespace(&mut self) {
        while let Some((_, ' ')) | Some((_, '\t')) | Some((_, '\r')) = self.peek_char() {
            self.next_char();
        }
    }

    /// Clears the buffer and pushes the current character onto it.
    fn start_buffer(&mut self) {
        self.buf.clear();
        self.buf.push(self.current.1);
    }

    /// Clears the buffer and returns the result.
    fn drain_buffer(&mut self) -> SmolStr {
        let s = SmolStr::new(&self.buf);
        self.buf.clear();
        s
    }

    /// Indicates that the character stream cursor is at the end.
    ///
    /// Important note about MultiPeek, when the iterator is at
    /// the last position, peek will return `None` instead of
    /// the final element.
    fn at_end(&self) -> bool {
        // Exclusive because source size is used a marker to
        // return an End-of-File token.
        self.current.0 > self.source_size
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
        let mut lexer = Lexer::from_str("System.print(\"Hello, world!\")");
        let chars = [(0, 'S'), (1, 'y'), (2, 's'), (3, 't'), (4, 'e'), (5, 'm'), (6, '.')];
        for (index, c) in &chars {
            assert_eq!(lexer.next_char(), Some((*index, *c)));
        }
    }

    #[test]
    fn test_lexer_next_token() {
        let mut lexer = Lexer::from_str("System.print(\"Hello, world!\")");

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
        let lexer = Lexer::from_str(&source);
        assert_eq!(lexer.into_tokens().len(), 7);
    }

    #[test]
    fn test_lex_number_lit() {
        let mut lexer = Lexer::from_str("1 + 2 * 3");
        assert_eq!(lexer.next_token().map(|t| t.ty), Some(TokenType::Number));
        assert_eq!(lexer.next_token().map(|t| t.ty), Some(TokenType::Add));
        assert_eq!(lexer.next_token().map(|t| t.ty), Some(TokenType::Number));
        assert_eq!(lexer.next_token().map(|t| t.ty), Some(TokenType::Mul));
        assert_eq!(lexer.next_token().map(|t| t.ty), Some(TokenType::Number));
    }
}
