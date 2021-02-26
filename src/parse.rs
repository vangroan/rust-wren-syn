use crate::{
    ast::{Expr, Notation, Syntax},
    lex::{Token, TokenType},
    BinaryOp, NumLit, UnaryOp,
};
use std::convert::{Infallible, TryFrom};
use std::{fmt, ops};

// type Precedence = i16;

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
enum Precedence {
    None = 0,
    Lowest = 1,
    Assignment = 2,    // =
    Conditional = 3,   // ?:
    LogicalOr = 4,     // ||
    LogicalAnd = 5,    // &&
    Equality = 6,      // == !=
    Is = 7,            // is
    Comparison = 8,    // < > <= >=
    BitwiseOr = 9,     // |
    BitwiseXor = 10,   // ^
    BitwiseAnd = 11,   // &
    BitwiseShift = 12, // << >>
    Range = 13,        // .. ...
    Term = 14,         // + -
    Factor = 15,       // * / %
    Unary = 16,        // - ! ~
    Call = 17,         // . () []
    Primary = 18,
}

impl Precedence {
    #[inline(always)]
    fn as_i32(&self) -> i32 {
        *self as i32
    }
}

impl TryFrom<i32> for Precedence {
    type Error = Infallible;

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Precedence::None),
            1 => Ok(Precedence::Lowest),
            2 => Ok(Precedence::Assignment),
            3 => Ok(Precedence::Conditional),
            4 => Ok(Precedence::LogicalOr),
            5 => Ok(Precedence::LogicalAnd),
            6 => Ok(Precedence::Equality),
            7 => Ok(Precedence::Is),
            8 => Ok(Precedence::Comparison),
            9 => Ok(Precedence::BitwiseOr),
            10 => Ok(Precedence::BitwiseXor),
            11 => Ok(Precedence::BitwiseAnd),
            12 => Ok(Precedence::BitwiseShift),
            13 => Ok(Precedence::Range),
            14 => Ok(Precedence::Term),
            15 => Ok(Precedence::Factor),
            16 => Ok(Precedence::Unary),
            17 => Ok(Precedence::Call),
            18 => Ok(Precedence::Primary),
            _ => Ok(Precedence::Primary),
        }
    }
}

impl fmt::Display for Precedence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.as_i32(), f)
    }
}

impl ops::Add<i32> for Precedence {
    type Output = Precedence;

    fn add(self, rhs: i32) -> Self::Output {
        Precedence::try_from(self.as_i32() + rhs).unwrap()
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Associativity {
    Left,
    Right,
}

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    statements: Vec<Syntax>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            statements: vec![],
        }
    }

    /// Parse the given token buffer as top-level Wren script file.
    ///
    /// TODO: Confusing name. Sounds like it should be accepting script as argument.
    pub fn parse_script(mut self) -> Vec<Syntax> {
        while !self.at_end() && self.peek().map(|t| t.ty != TokenType::EOF).unwrap_or_default() {
            self.definition();
        }

        self.statements
    }

    /// Parse a definition statement.
    ///
    /// These are statements that declare bindings like `var` and
    /// `class`, which may only appear at the top level of curly braced
    /// blocks.
    ///
    /// They are not allowed in places like `if` conditional statement
    /// branches that do not have curly braces.
    fn definition(&mut self) {
        println!("definition");
        // TODO: Class
        // TODO: Foreign
        // TODO: Import
        // TODO: Var
        // Else: Simple Statement
        self.statement();
    }

    /// Parse a simple statement.
    ///
    /// Statements can only appear at the top level of the curly braces.
    /// Simple statements exclude variable binding statements like
    /// `var` and `class`, because these are not allowed in places
    /// like the branches of `if` conditional statement that don't
    /// have curly braces.
    fn statement(&mut self) {
        println!("statement");
        // TODO: Break
        // TODO: Continue
        // TODO: For
        // TODO: If
        // TODO: Return
        // Else: Expression statement
        let expr = Syntax::Expr(self.expression());
        self.statements.push(expr);
    }

    fn expression(&mut self) -> Expr {
        println!("expression");

        // Pratt parser is initialised with lowest possible precedence.
        self.parse_precedence(Precedence::None)
    }

    /// Entrypoint for the top-down precedence parser.
    ///
    /// The implementation is a straight forward Pratt parser.
    fn parse_precedence(&mut self, precedence: Precedence) -> Expr {
        println!("parse_precedence({}) {:?}", precedence, self.peek());

        let token = self.next_token().expect("Expression expected");
        let mut left = Some(self.parse_prefix(token));

        while precedence <= self.peek_precedence() {
            // There is no expression right of the last one, so we
            // just return what we have.
            if let Some(TokenType::EOF) | None = self.peek().map(|token| token.ty) {
                return left.unwrap();
            }

            let token = self.next_token().unwrap();
            left = Some(self.parse_infix(left.take().unwrap(), token))
        }

        left.take().unwrap()
    }

    /// Parse a prefix token in an expression.
    ///
    /// This function is analogous to a parselet.
    fn parse_prefix(&mut self, token: Token) -> Expr {
        use TokenType as T;
        println!("parse_prefix {:?}", token);

        match token.ty {
            T::Number => {
                println!("parse_prefix() -> NumLit");

                Expr::Num(NumLit {
                    token,
                    notation: Notation::Decimal,
                })
            }
            T::Sub => {
                // Negate
                println!("parse_prefix() -> UnaryOp {{ operator: TokenType::Sub }}");
                let precedence = Self::precedence(token.ty);
                let right = self.parse_precedence(precedence + 1);
                Expr::UnOp(UnaryOp {
                    operand: token,
                    rhs: Box::new(right),
                })
            }
            // When this match fails, it means there is no parselet for the token, meaning
            // some invalid token is in an unexpected position.
            _ => panic!("Expected expression"),
        }
    }

    fn parse_infix(&mut self, left: Expr, operand: Token) -> Expr {
        use TokenType as T;
        println!("parse_infix {:?} ... {:?} ... {:?}", left, operand, self.peek());

        let precedence = Self::precedence(operand.ty);

        // Associativity is handled by adjusting the precedence.
        // Left associativity is achieved by increasing the precedence
        // by 1. This increases the threshold that any infix expressions
        // to our right must exceed.
        //
        // Right associativity can be achieved by keeping
        // the precedence the same, thus keeping the threshold any
        // subsequent infix expression need to exceed to be parsed.
        //
        // Wren doesn't appear to have right-associative operators.
        let associativity = if Self::associativity(operand.ty) == Associativity::Left {
            1
        } else {
            0
        };

        // Recurse back into expression parser to handle
        // the right hand side.
        //
        // The left hand side will wait for us here on
        // the call stack.
        let right = self.parse_precedence(precedence + associativity);

        match operand.ty {
            T::Add | T::Sub | T::Mul | T::Div => Expr::BinOp(BinaryOp {
                operand,
                lhs: Box::new(left),
                rhs: Box::new(right),
            }),
            _ => panic!("Expected expression"),
        }
    }

    /// Associativity is the precedence tie-breaker.
    ///
    /// Wren doesn't seem to have any right-associative operators.
    fn associativity(_token_ty: TokenType) -> Associativity {
        Associativity::Left
    }

    /// Get the precedence of the given token type in the context
    /// of the expression parser.
    fn precedence(token_ty: TokenType) -> Precedence {
        use TokenType as T;

        match token_ty {
            T::Number => Precedence::None,
            T::Add | T::Sub => Precedence::Term,
            T::Mul | T::Div => Precedence::Factor,
            _ => Precedence::None,
        }
    }

    /// Retrieve the precedence of the current token.
    fn peek_precedence(&self) -> Precedence {
        self.peek()
            .map(|token| Self::precedence(token.ty))
            .unwrap_or_else(|| Precedence::None)
    }

    /// Consumes the current token regardless of type.
    ///
    /// Returns `None` when the cursor is at the end of the token stream.
    fn next_token(&mut self) -> Option<Token> {
        let t = self.tokens.get(self.cursor).cloned();
        self.cursor += 1;
        t
    }

    /// Consumes the current token if it matches the given token type.
    ///
    /// Returns true when matched. Returns false when token types
    /// do not match, or the token stream is at the end.
    fn match_token(&mut self, token_ty: TokenType) -> bool {
        match self.peek() {
            Some(token) => {
                let is_match = token.ty == token_ty;
                if is_match {
                    self.cursor += 1;
                }
                is_match
            }
            None => false,
        }
    }

    /// Return the current token with advancing the cursor.
    ///
    /// The consumed token must match the given token type, otherwise
    /// a parsing error is returned.
    fn consume(&mut self, token_ty: TokenType) -> Option<Token> {
        let t = self.tokens.get(self.cursor).cloned();
        if let Some(token) = &t {
            if token.ty != token_ty {
                // TODO: Return parsing error.
                panic!(
                    "Parsing error: expected token '{:?}' but encountered '{:?}'",
                    token_ty, token.ty
                );
            }
        }

        self.cursor += 1;
        t
    }

    /// Return the current token without advancing the cursor.
    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.cursor).cloned()
    }

    /// Return the token offset from the current cursor without advancing the cursor.
    fn look_ahead(&mut self, offset: i32) -> Option<Token> {
        self.tokens.get((self.cursor as i32 + offset) as usize).cloned()
    }

    /// Returns true when the cursor is at the end of the token stream.
    fn at_end(&self) -> bool {
        self.cursor >= self.tokens.len()
    }
}
