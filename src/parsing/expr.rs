//! Expression parsing.
use super::{Parse, ParseResult, SyntaxError};
use crate::{
    lex::TokenStream,
    token::{Token, TokenType},
};
use std::{
    convert::{Infallible, TryFrom},
    fmt, ops,
};

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

    /// Get the precedence of the given token type in the context
    /// of the expression parser.
    fn of(token_ty: TokenType) -> Precedence {
        use crate::token::TokenType as T;

        match token_ty {
            T::Number | T::Field | T::StaticField => Precedence::None,
            T::Add | T::Sub => Precedence::Term,
            T::Mul | T::Div => Precedence::Factor,
            T::Eq => Precedence::Assignment,
            _ => Precedence::None,
        }
    }
}

impl TryFrom<i32> for Precedence {
    type Error = Infallible;

    #[rustfmt::skip]
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        use Precedence as P;
        match value {
            0  => Ok(P::None),
            1  => Ok(P::Lowest),
            2  => Ok(P::Assignment),
            3  => Ok(P::Conditional),
            4  => Ok(P::LogicalOr),
            5  => Ok(P::LogicalAnd),
            6  => Ok(P::Equality),
            7  => Ok(P::Is),
            8  => Ok(P::Comparison),
            9  => Ok(P::BitwiseOr),
            10 => Ok(P::BitwiseXor),
            11 => Ok(P::BitwiseAnd),
            12 => Ok(P::BitwiseShift),
            13 => Ok(P::Range),
            14 => Ok(P::Term),
            15 => Ok(P::Factor),
            16 => Ok(P::Unary),
            17 => Ok(P::Call),
            18 => Ok(P::Primary),
            _  => Ok(P::None),
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

/// Associativity is the precedence tie-breaker.
#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Associativity {
    Left,
    Right,
}

impl Associativity {
    fn of(token_ty: TokenType) -> Associativity {
        if token_ty == TokenType::Eq {
            Associativity::Right
        } else {
            Associativity::Left
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    /// Number literal.
    Num(NumLit),
    Str(StrLit),
    UnOp(UnaryOp),
    BinOp(BinaryOp),
}

/// Number literal.
#[derive(Debug)]
pub struct NumLit {
    pub token: Token,
    pub notation: Notation,
    /// Constant value parsed from the number.
    ///
    /// All numbers in Wren are 64-bit floats.
    pub value: f64,
}

/// String literal.
#[derive(Debug)]
pub struct StrLit {
    pub token: Token,
    pub value: String,
}

/// Format in which the numeral was written.
#[derive(Debug)]
pub enum Notation {
    Decimal,
    Hexadecimal,
    Scientific,
}

/// Arithmetic operation with an expression on the right side.
#[derive(Debug)]
pub struct UnaryOp {
    pub operator: Token,
    pub rhs: Box<Expr>,
}

/// Arithmetic operation with an expression on either side.
#[derive(Debug)]
pub struct BinaryOp {
    pub operator: Token,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Parse for Expr {
    fn parse(input: &mut TokenStream) -> ParseResult<Expr> {
        Expr::parse_precedence(input, Precedence::None)
    }
}

/// Recursive parsing methods.
impl Expr {
    /// Entrypoint for the top-down precedence parser.
    ///
    /// The implementation is a straight forward Pratt parser.
    fn parse_precedence(input: &mut TokenStream, precedence: Precedence) -> ParseResult<Expr> {
        println!("Expr::parse_precedence");
        let token = input.next_token().ok_or_else(|| SyntaxError {
            msg: "expression expected".to_string(),
        })?;

        // The current expression node is wrapped in `Option`
        // so that it can be moved into the recursive parser,
        // and the stack value replaced with the parsing result.
        let mut left = Some(Self::parse_prefix(input, token)?);

        input.reset_peek();

        while precedence <= input.peek().map(|t| Precedence::of(t.ty)).unwrap_or(Precedence::None) {
            // Peek advances a peek pointer inside the token stream,
            // so it needs to be reset otherwise we are inadvertently
            // looking further ahead.
            input.reset_peek();

            // There is no expression right of the last one, so we
            // just return what we have.
            if let Some(TokenType::EOF) | None = input.peek().map(|token| token.ty) {
                return Ok(left.take().unwrap());
            }

            let token = input.next_token().ok_or_else(|| SyntaxError {
                msg: "expression expected".to_string(),
            })?;
            left = Some(Self::parse_infix(input, left.take().unwrap(), token)?);
        }

        Ok(left.take().unwrap())
    }

    /// Parse a prefix token in an expression.
    ///
    /// This function is analogous to a parselet.
    fn parse_prefix(input: &mut TokenStream, token: Token) -> ParseResult<Expr> {
        use crate::token::TokenType as T;

        match token.ty {
            T::Number => Ok(Expr::Num(Self::parse_number_literal(token)?)),
            T::Field | T::StaticField => todo!("parse_field"),
            T::Sub => {
                // Negate
                let right = Self::parse_precedence(input, Precedence::Unary)?;
                Ok(Expr::UnOp(UnaryOp {
                    operator: token,
                    rhs: Box::new(right),
                }))
            }
            // When this match fails, it means there is no parselet for the token, meaning
            // some invalid token is in an unexpected position.
            _ => Err(SyntaxError {
                msg: "expression expected".to_string(),
            }
            .into()),
        }
    }

    /// Parse an infix, postfix or mixfix operator.
    ///
    /// Includes non-obvious tokens like opening parentheses `(`.
    fn parse_infix(input: &mut TokenStream, left: Expr, token: Token) -> ParseResult<Expr> {
        use TokenType as T;

        let precedence = Precedence::of(token.ty);

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
        let binding_power = if Associativity::of(token.ty) == Associativity::Left {
            1
        } else {
            0
        };

        // Recurse back into expression parser to handle
        // the right hand side.
        //
        // The left hand side will wait for us here on
        // the call stack.
        let right = Self::parse_precedence(input, precedence + binding_power)?;

        match token.ty {
            T::Add | T::Sub | T::Mul | T::Div | T::Eq => Ok(Expr::BinOp(BinaryOp {
                operator: token,
                lhs: Box::new(left),
                rhs: Box::new(right),
            })),
            _ => Err(SyntaxError {
                msg: "infix expression expected".to_string(),
            }
            .into()),
        }
    }

    /// Parse a number literal token to a number syntax node.
    ///
    /// The value of the number is parsed and returned.
    fn parse_number_literal(token: Token) -> ParseResult<NumLit> {
        assert_eq!(token.ty, TokenType::Number);

        // TODO: Get notation from lexer to decide how to parse number.
        //       Hex: i64::from_str_radix("FF", 16);
        //       Science: "3e-7".parse::<f64>();

        // Parse number literal to a constant value.
        let value: f64 = token
            .lit
            .as_ref()
            .and_then(|lit| lit.number())
            .ok_or_else(|| SyntaxError {
                msg: "number literal has no literal string".to_string(),
            })?
            .parse::<f64>()
            .map_err(|err| SyntaxError {
                msg: format!("{}", err),
            })?;

        Ok(NumLit {
            token,
            notation: Notation::Decimal,
            value,
        })
    }
}

/// Helpers for extracting variants in call chains, like `and_then`.
impl Expr {
    #[inline]
    pub fn number(&self) -> Option<&NumLit> {
        match self {
            Expr::Num(number) => Some(number),
            _ => None,
        }
    }

    #[inline]
    pub fn string(&self) -> Option<&StrLit> {
        match self {
            Expr::Str(string) => Some(string),
            _ => None,
        }
    }

    #[inline]
    pub fn unary(&self) -> Option<&UnaryOp> {
        match self {
            Expr::UnOp(unary) => Some(unary),
            _ => None,
        }
    }

    #[inline]
    pub fn binary(&self) -> Option<&BinaryOp> {
        match self {
            Expr::BinOp(binary) => Some(binary),
            _ => None,
        }
    }
}
