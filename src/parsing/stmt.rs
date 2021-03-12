//! Definition statements. Eg: `class`, `foreign`, `import`, `var`.
use super::{
    comment::Comment,
    errors::{ParseResult, SyntaxError},
    expr::Expr,
    Parse,
};
use crate::{
    lex::TokenStream,
    token::{Ident, KeywordType, Token, TokenType},
};

/// Definition statement.
///
/// These are statements that declare bindings like `var` and
/// `class`, which may only appear at the top level of curly braced
/// blocks.
///
/// They are not allowed in places like `if` conditional statement
/// branches that do not have curly braces.
///
/// The following *invalid*:
///
/// ```wren
/// if (true) var x = 1
/// if (true) { var x = 1 }
/// if (true)
///   var x = 1
/// ```
///
/// The following is *valid*:
///
/// ```wren
/// if (true) {
///   var x = 1
/// }
/// ```
#[derive(Debug)]
pub enum DefStmt {
    Class(ClassDef),
    Foreign,
    Import,
    Var,
    Simple(SimpleStmt),
}

/// Class definition statement.
#[derive(Debug)]
pub struct ClassDef {
    /// Class name.
    pub ident: Ident,
    /// Base class that this class is inheriting from.
    pub parent: Option<Ident>,
    /// TODO: Member methods.
    pub members: ClassMembers,
}

/// Statements that belong to a class.
#[derive(Debug, Default)]
pub struct ClassMembers {
    pub fields: Vec<Field>,
    pub construct: Option<Method>,
    pub props: Vec<()>,
    pub methods: Vec<Method>,
    pub ops: Vec<()>,
}

#[derive(Debug)]
pub enum ClassMember {
    Comment(Comment),
    Constructor(Method),
    Property(Method),
    Method(Method),
    Operator(Method),
}

/// Method declaration, inside a class body.
#[derive(Debug)]
pub struct Method {
    /// Comments that immediately preceded the
    /// method definition.
    pub comments: Vec<Comment>,
    pub modifiers: Modifiers,
    pub sig: Signature,
    /// Class fields are declared in methods.
    pub fields: Vec<Field>,
    /// Statements contained in the method's body.
    ///
    /// Same parsing rules are `Module`.
    pub body: (),
}

#[derive(Debug)]
pub struct Modifiers {
    pub is_foreign: bool,
    pub is_static: bool,
    pub is_construct: bool,
}

#[derive(Debug)]
pub struct Param {
    pub token: Token,
    pub ident: Ident,
}

#[derive(Debug)]
pub struct Field {
    pub is_static: bool,
    pub ident: Ident,
}

#[derive(Debug)]
pub struct Signature {
    /// Method name can be either an identifier or operators.
    pub ident: Option<Ident>,
    /// List of parameters located between parentheses with
    /// a method's signature.
    pub params: Vec<Param>,
    /// Number of parameters.
    pub arity: u8,
}

/// Simple statement.
///
/// Definition statements can only appear at the top level of the curly braces.
/// Simple statements exclude variable binding statements like
/// `var` and `class`, because these are not allowed in places
/// like the branches of `if` conditional statement that don't
/// have curly braces.
#[derive(Debug)]
pub enum SimpleStmt {
    Break,
    Continue,
    For,
    If,
    Return,
    /// FIXME: Is this the right place to put block body level comments?
    ///        We will probably only find out when implementing the language server.
    Comment(Comment),
    /// Expression statement.
    Expr(Expr),
}

impl Parse for DefStmt {
    fn parse(input: &mut TokenStream) -> ParseResult<Self> {
        println!("DefStmt::parse");
        use KeywordType as K;
        use TokenType as T;

        input.match_lines();

        if let Some(token) = input.peek() {
            if let T::Keyword(keyword) = token.ty {
                match keyword {
                    K::Class => Ok(Self::Class(ClassDef::parse(input)?)),
                    K::Foreign => todo!("foreign class definition statement"),
                    K::Import => todo!("import definition statement"),
                    K::Var => todo!("var definition statement"),
                    _ => Err(SyntaxError {
                        msg: "unexpected keyword".to_string(),
                    }
                    .into()),
                }
            } else {
                Ok(Self::Simple(SimpleStmt::parse(input)?))
            }
        } else {
            Err(SyntaxError {
                msg: "unexpected end of file".to_string(),
            }
            .into())
        }
    }
}

impl Parse for ClassDef {
    fn parse(input: &mut TokenStream) -> ParseResult<Self> {
        println!("ClassDef::parse");
        use KeywordType as K;
        use TokenType as T;

        // Consume class keyword
        input.consume(T::Keyword(K::Class))?;

        let cls_token = input.consume(T::Ident).map_err(|_| SyntaxError {
            msg: "class name expected".to_string(),
        })?;
        let cls_ident = cls_token.ident.ok_or_else(|| SyntaxError {
            msg: "class name token has no identifier".to_string(),
        })?;

        // When the class name is followed by the keyword `is`,
        // then it's inheriting a another class.
        //
        // Wren only has single inheritance, so just one parent.
        if input.match_token(T::Keyword(K::Is)) {
            println!("ClassDef derived class");
            // Keyword `is` must be followed by parent class' name.
            let parent_token = input.consume(T::Ident).map_err(|_| SyntaxError {
                msg: "parent class name expected".to_string(),
            })?;
            let parent_ident = parent_token.ident.ok_or_else(|| SyntaxError {
                msg: "parent class name token has no identifier".to_string(),
            })?;
            // Simple class definition with no inheritance.
            Ok(Self {
                ident: cls_ident,
                parent: Some(parent_ident),
                // TODO: Class members
                members: ClassMembers::parse(input)?,
            })
        } else {
            println!("ClassDef simple class");
            // Simple class definition with no inheritance.
            Ok(Self {
                ident: cls_ident,
                parent: None,
                // TODO: Class members
                members: ClassMembers::parse(input)?,
            })
        }
    }
}

impl Parse for ClassMembers {
    fn parse(input: &mut TokenStream) -> ParseResult<Self> {
        println!("ClassMembers::parse");
        use TokenType as T;

        // Class body is always opened by a brace.
        input.consume(T::LeftBrace)?;
        input.match_lines();

        let mut construct: Option<Method> = None;
        let mut methods = vec![];
        let mut comments = vec![];

        while !input.match_token(T::RightBrace) {
            input.reset_peek();
            let token_ty = input.peek().map(|t| t.ty).ok_or_else(|| SyntaxError {
                msg: "unexpected end of file".to_string(),
            })?;

            match token_ty {
                T::EOF => {
                    return Err(SyntaxError {
                        msg: "unexpected end-of-file".to_string(),
                    }
                    .into())
                }
                T::Newline => {
                    input.next_token();
                    continue;
                }
                T::CommentLine | T::CommentLeft => {
                    comments.push(Comment::parse(input)?);
                }
                _ => {
                    let mut method = Method::parse(input)?;

                    // Flush all the preceding comments.
                    method.comments.extend(comments.drain(..));

                    if method.modifiers.is_construct {
                        construct = Some(method);
                    } else {
                        methods.push(method);
                    }

                    // Don't require a newline after the last member definition.
                    if input.match_token(T::RightBrace) {
                        break;
                    }

                    // Members must be separated with new lines.
                    input.match_token(T::Newline);
                }
            }
        }

        // Comments are allowed immediately after class definition.
        // But we don't add them to the AST yet.
        // FIXME: Append trailing comments to class definition.
        Comment::ignore(input);

        // Don't allow any other statements or expressions after
        // class definition.
        if !input.match_token(T::Newline) && !input.match_token(TokenType::EOF) {
            return Err(SyntaxError {
                msg: "expected new line after class definition".to_string(),
            }
            .into());
        }

        Ok(ClassMembers {
            construct,
            methods,
            ..Default::default()
        })
    }
}

impl Parse for Method {
    fn parse(input: &mut TokenStream) -> ParseResult<Self> {
        println!("Method::parse");
        use KeywordType as K;
        use TokenType as T;

        let modifiers = Modifiers {
            is_foreign: input.match_token(T::Keyword(K::Foreign)),
            is_static: input.match_token(T::Keyword(K::Static)),
            is_construct: input.match_token(T::Keyword(K::Construct)),
        };

        if modifiers.is_construct && modifiers.is_static {
            return Err(SyntaxError {
                msg: "constructor cannot be static".to_string(),
            }
            .into());
        }

        let next_token_ty = input.peek().map(|t| t.ty).ok_or_else(|| SyntaxError {
            msg: "expected method name".to_string(),
        })?;

        // Methods in Wren can start with various tokens.
        // Keywords like `foreign` or `static`, method name
        // as an identifier, or any number of operators like
        // `+`, '[' or '='.
        match next_token_ty {
            T::Ident => Self::parse_named_method(input, modifiers),
            _ => Self::parse_operator(input),
        }
    }
}

impl Method {
    /// Parse a method with a name, and optionally a parameter list().
    ///
    /// A named method can be a property *getter*, *setter* or *function*.
    fn parse_named_method(input: &mut TokenStream, modifiers: Modifiers) -> ParseResult<Self> {
        println!("Method::parse_named_method");
        use TokenType as T;
        let maybe_ident = input.consume(TokenType::Ident)?.ident;
        println!("maybe_ident: {:?}", maybe_ident);

        if input.match_token(T::Eq) {
            // Property setter.
            if modifiers.is_construct {
                return Err(SyntaxError {
                    msg: "constructor cannot be a setter".to_string(),
                }
                .into());
            }
            todo!("parse setter's one param");
        } else {
            // Method parameters are optional.
            input.reset_peek();
            let next_token = input.peek().ok_or_else(|| SyntaxError {
                msg: "expected method parameters or '{'".to_string(),
            })?;

            let params = if next_token.ty == T::LeftParen {
                Self::parse_paren(input)?
            } else {
                println!("No params token type: {:?}", next_token.ty);
                if modifiers.is_construct {
                    return Err(SyntaxError {
                        msg: "constructor cannot be a getter".to_string(),
                    }
                    .into());
                }
                Default::default()
            };

            // Foreign methods don't have bodies.
            if modifiers.is_foreign {
                // Foreign method has no body.
                // return Ok(Method {
                //     modifiers,
                //     params,
                //     fields: Default::default(),
                // })
                todo!()
            } else {
                // TODO: Parse body
                // input.match_token(T::LeftBrace);
                // input.match_token(T::RightBrace);

                Ok(Method {
                    comments: Default::default(),
                    modifiers,
                    sig: Signature {
                        ident: maybe_ident,
                        arity: params.len() as u8,
                        params,
                    },
                    fields: Default::default(),
                    body: Self::parse_body(input)?,
                })
            }
        }
    }

    fn parse_operator(_input: &mut TokenStream) -> ParseResult<Self> {
        todo!("parse operator methods")
    }

    fn parse_paren(input: &mut TokenStream) -> ParseResult<Vec<Param>> {
        println!("Method::parse_paren");
        use TokenType as T;

        input.consume(T::LeftParen)?;

        // Parentheses can be empty.
        if input.match_token(T::RightParen) {
            return Ok(Default::default());
        }

        let params = Self::parse_params(input)?;

        input.consume(T::RightParen)?;

        Ok(params)
    }

    fn parse_params(input: &mut TokenStream) -> ParseResult<Vec<Param>> {
        println!("Method::parse_params");
        let mut params = vec![];

        loop {
            input.match_lines();

            // TODO: Validate number of parameters against MAX_PARAMETERS
            let token = input.consume(TokenType::Ident)?;
            params.push(Param {
                ident: token.ident.clone().ok_or_else(|| SyntaxError {
                    msg: "parameter token has no identifier".to_string(),
                })?,
                token,
            });

            if !input.match_token(TokenType::Comma) {
                break;
            }
        }

        Ok(params)
    }

    /// TODO: Parse method body
    fn parse_body(input: &mut TokenStream) -> ParseResult<()> {
        println!("Method::parse_body");
        use TokenType as T;

        input.consume(T::LeftBrace)?;

        while !input.match_token(T::RightBrace) {
            if let Some(token) = input.peek() {
                match token.ty {
                    T::EOF => {
                        return Err(SyntaxError {
                            msg: "unexpected end-of-file".to_string(),
                        }
                        .into())
                    }
                    T::RightBrace => {
                        // Prevent terminal token from being consumed.
                        input.match_token(T::RightBrace);
                        break;
                    }
                    _ => {
                        // TODO: Parse statements in method body
                        println!("########### IGNORE");
                        input.next_token();
                    }
                }
            } else {
                return Err(SyntaxError {
                    msg: "unexpected end-of-file".to_string(),
                }
                .into());
            }

            // Last statement doesn't need to be terminated with a new line.
            if input.match_token(T::RightBrace) {
                println!("RIGHT BRACE BREAK");
                break;
            }
        }

        // Comments are allowed immediately after method definition.
        // Comment::ignore(input);

        // Don't allow any other statements or expressions after
        // class definition.
        if !input.match_token(T::Newline) {
            return Err(SyntaxError {
                msg: "expected new line after method definition".to_string(),
            }
            .into());
        }

        println!("Method::parse_body done");

        Ok(())
    }
}

impl Parse for SimpleStmt {
    fn parse(input: &mut TokenStream) -> ParseResult<Self> {
        println!("SimpleStmt::parse");
        use TokenType as T;

        input.reset_peek();

        // TODO: Break
        // TODO: Continue
        // TODO: For
        // TODO: If
        // TODO: Return
        // Expression statement
        match input.peek().map(|t| t.ty).ok_or_else(|| SyntaxError {
            msg: "unexpected end of file".to_string(),
        })? {
            T::CommentLine | T::CommentLeft => {
                Ok(SimpleStmt::Comment(Comment::parse(input)?))
            }
            _ => Expr::parse(input).map(SimpleStmt::Expr),
        }
    }
}
