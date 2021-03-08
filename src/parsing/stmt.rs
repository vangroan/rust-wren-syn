//! Definition statements. Eg: `class`, `foreign`, `import`, `var`.
use super::{
    errors::{ParseResult, SyntaxError},
    Parse,
};
use crate::{
    lex::TokenStream,
    token::{Ident, KeywordType, TokenType},
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
    pub construct: Option<()>,
    pub fields: Vec<()>,
    pub props: Vec<()>,
    pub methods: Vec<()>,
    pub ops: Vec<()>,
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
    /// Expression statement.
    Expr,
}

impl Parse for DefStmt {
    fn parse(input: &mut TokenStream) -> ParseResult<Self> {
        use KeywordType as K;
        use TokenType as T;

        input.reset_peek();

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
        use KeywordType as K;
        use TokenType as T;

        // Consume class keyword
        input.match_token(T::Keyword(K::Class));

        let cls_token = input.consume(T::Ident).ok_or_else(|| SyntaxError {
            msg: "class name expected".to_string(),
        })??;
        let cls_ident = cls_token.ident.ok_or_else(|| SyntaxError {
            msg: "class name token has no identifier".to_string(),
        })?;

        // When the class name is followed by the keyword `is`,
        // then it's inheriting a another class.
        //
        // Wren only has single inheritance, so just one parent.
        if input.match_token(T::Keyword(K::Is)) {
            // Keyword `is` must be followed by parent class' name.
            let parent_token = input.consume(T::Ident).ok_or_else(|| SyntaxError {
                msg: "parent class name expected".to_string(),
            })??;
            let parent_ident = parent_token.ident.ok_or_else(|| SyntaxError {
                msg: "parent class name token has no identifier".to_string(),
            })?;
            // Simple class definition with no inheritance.
            Ok(Self {
                ident: cls_ident,
                parent: Some(parent_ident),
                // TODO: Class members
                members: ClassMembers::default(),
            })
        } else {
            // Simple class definition with no inheritance.
            Ok(Self {
                ident: cls_ident,
                parent: None,
                // TODO: Class members
                members: ClassMembers::default(),
            })
        }
    }
}

impl Parse for SimpleStmt {
    fn parse(_input: &mut TokenStream) -> ParseResult<Self> {
        todo!()
    }
}
