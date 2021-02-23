use crate::{
    ast::{Expr, Notation, Syntax},
    lex::{Token, TokenType},
    BinaryOp, NumLit,
};

type Precedence = i16;

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
        while !self.at_end() {
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
        self.parse_precedence(0)
    }

    /// Entrypoint for the top-down precedence parser.
    ///
    /// The implementation is a straight forward Pratt parser.
    fn parse_precedence(&mut self, precedence: Precedence) -> Option<Expr> {
        println!("parse_precedence({})", precedence);

        let token = match self.peek() {
            Some(t) => t,
            None => {
                // No tokens left, parsing done.
                return None;
            }
        };

        let left = self.parse_prefix(token);

        // Parsing the prefix should have advanced the cursor.
        let token = match self.peek() {
            Some(t) => t,
            None => {
                // No tokens left, parsing done.
                return left;
            }
        };

        self.next_token();
        Some(self.parse_infix(left, token))
    }

    /// Parse a prefix token in an expression.
    ///
    /// This function is analogous to a parselet.
    fn parse_prefix(&mut self, operand: Token) -> Expr {
        use TokenType as T;
        println!("parse_prefix {:?}", self.peek());

        self.next_token().map(|token| {
            match token.ty {
                T::Number => {
                    println!("parse_prefix() -> NumLit");

                    Expr::Num(NumLit {
                        token,
                        notation: Notation::Decimal,
                    })
                }
                // When this match fails, it means there is no parselet for the token, meaning
                // some invalid token is in an unexpected position.
                _ => panic!("Expected expression"),
            }
        })
    }

    fn parse_infix(&mut self, left: Expr, operand: Token) -> Expr {
        use TokenType as T;
        println!("parse_infix {:?} {:?} {:?}", left, operand, self.peek());

        // Recurse back into expression parser to handle
        // the right hand side.
        //
        // The left hand side will wait for us here on
        // the call stack.
        let right = self.parse_precedence(0);

        match operand.ty {
            T::Add | T::Sub | T::Mul | T::Div => Expr::BinOp(BinaryOp {
                operand,
                lhs: Box::new(left),
                rhs: Box::new(right),
            }),
            _ => panic!("Expected expression"),
        }
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
