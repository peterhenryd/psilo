use std::fmt::{Debug, Display, Formatter};
use logos::Span;
use derive_more::Display;

#[derive(Debug)]
pub struct Module<'a> {
    pub absolute_name: Vec<&'a str>,
    pub use_decls: Vec<Spanned<Name<'a>>>,
    pub symbols: Vec<Symbol<'a>>,
}

pub type Node<'a, T> = Typed<'a, Spanned<T>>;

impl<'a, T> Node<'a, T> {
    pub fn new(span: Span, ty: Option<Type<'a>>, val: T) -> Self {
        Typed(ty, Spanned(span, val))
    }
}

pub struct Typed<'a, T>(pub Option<Type<'a>>, pub T);

impl<T: Debug> Debug for Typed<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Typed(")?;
        self.0.fmt(f)?;
        f.write_str(", ")?;
        self.1.fmt(f)?;
        f.write_str(")")
    }
}

impl<T: Clone> Clone for Typed<'_, T> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}


pub struct Spanned<T>(pub Span, pub T);

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Spanned(")?;
        self.0.fmt(f)?;
        f.write_str(", ")?;
        self.1.fmt(f)?;
        f.write_str(")")
    }
}

impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

impl<T: Eq> PartialEq<Self> for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

impl <T: Eq> Eq for Spanned<T> {}

impl<T> Spanned<T> {
    #[inline]
    pub fn into_node<'a>(self) -> Node<'a, T> {
        Node::new(self.0, None, self.1)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type<'a> {
    Sint(Option<u16>),
    Uint(Option<u16>),
    Float,
    F32,
    F64,
    Str,
    Char,
    Bool,
    Unit,
    Ref {
        is_mut: bool,
        ty: Box<Type<'a>>,
    },
    Struct(Spanned<Name<'a>>),
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Sint(i) => {
                f.write_str("s")?;
                if let Some(i) = i {
                    Display::fmt(i, f)?;
                }

                Ok(())
            },
            Type::Uint(i) => {
                f.write_str("u")?;
                if let Some(i) = i {
                    Display::fmt(i, f)?;
                }

                Ok(())
            },
            Type::Float => f.write_str("float"),
            Type::F32 => f.write_str("f32"),
            Type::F64 => f.write_str("f64"),
            Type::Str => f.write_str("str"),
            Type::Char => f.write_str("char"),
            Type::Bool => f.write_str("bool"),
            Type::Unit => f.write_str("()"),
            Type::Ref { is_mut, ty } => {
                f.write_str("&")?;
                if *is_mut {
                    f.write_str("mut ")?;
                }
                Display::fmt(ty, f)
            },
            Type::Struct(name) => f.write_str(name.1.0.join("::").as_str()),
        }
    }
}

#[derive(Debug)]
pub struct FunctionSignature<'a> {
    pub arguments: Vec<FunctionArgument<'a>>,
    pub return_ty: Type<'a>
}

#[derive(Debug)]
pub struct FunctionArgument<'a> {
    pub is_mut: bool,
    pub name: &'a str,
    pub ty: Type<'a>,
}

#[derive(Debug)]
pub struct Function<'a> {
    pub visibility: Visibility,
    pub name: &'a str,
    pub signature: FunctionSignature<'a>,
    pub body: Spanned<Block<'a>>,
}

#[derive(Debug)]
pub struct Block<'a>(pub Vec<Stmt<'a>>);

#[derive(Debug, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct Name<'a>(pub Vec<&'a str>);

#[derive(Debug)]
pub enum Stmt<'a> {
    Expr(Node<'a, Expr<'a>>),
    Return(Node<'a, Expr<'a>>),
    Break(Node<'a, Expr<'a>>),
    Continue,
    Let {
        binding: &'a str,
        is_mut: bool,
        initial_value: Node<'a, Expr<'a>>
    },
    While {
        condition: Node<'a, Expr<'a>>,
        block: Spanned<Block<'a>>
    },
}

#[derive(Debug)]
pub struct WhenBranch<'a> {
    pub condition: Node<'a, Expr<'a>>,
    pub result: Node<'a, Expr<'a>>
}

#[derive(Debug)]
pub enum Expr<'a> {
    Unit,
    CallFunction {
        name: Spanned<Name<'a>>,
        arguments: Vec<Node<'a, Expr<'a>>>
    },
    Block(Block<'a>),
    Literal(Literal<'a>),
    Binary {
        op: BinaryOp,
        lhs: Box<Node<'a, Expr<'a>>>,
        rhs: Box<Node<'a, Expr<'a>>>
    },
    Unary {
        op: UnaryOp,
        expr: Box<Node<'a, Expr<'a>>>,
    },
    When {
        matching: Box<Node<'a, Expr<'a>>>,
        branches: Vec<WhenBranch<'a>>,
        default: Box<Node<'a, Expr<'a>>>
    },
    If {
        condition: Box<Node<'a, Expr<'a>>>,
        then: Spanned<Block<'a>>,
        default: Option<Box<Node<'a, Expr<'a>>>>,
    },
    Loop(Block<'a>),
    Ctx(&'a str),
    Field(Box<Node<'a, Expr<'a>>>, &'a str),
}

#[derive(Debug, Copy, Clone, Display)]
pub enum BinaryOp {
    #[display(fmt = "=")]
    Eq,
    #[display(fmt = "!=")]
    Neq,
    #[display(fmt = "&&")]
    LogAnd,
    #[display(fmt = "||")]
    LogOr,
    #[display(fmt = "&")]
    And,
    #[display(fmt = "|")]
    Or,
    #[display(fmt = "^")]
    Xor,
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "/")]
    Div,
    #[display(fmt = "%")]
    Rem,
    #[display(fmt = ">")]
    Greater,
    #[display(fmt = "<")]
    Less,
    #[display(fmt = ">=")]
    GreaterEq,
    #[display(fmt = "<=")]
    LessEq,
    #[display(fmt = "<<")]
    Shl,
    #[display(fmt = ">>")]
    Shr,
}

#[derive(Debug, Copy, Clone, Display)]
pub enum UnaryOp {
    #[display(fmt = "-")]
    Neg,
    #[display(fmt = "!")]
    Not,
    #[display(fmt = "&{}", "if *is_mut { \"mut\" } else { \"\" }")]
    Ref { is_mut: bool },
    #[display(fmt = "*")]
    Deref,
    #[display(fmt = "?")]
    Try,
}

#[derive(Debug)]
pub enum Literal<'a> {
    Str(&'a str),
    Int(u128),
    Float(f64),
    Char(char),
    Bool(bool),
}

#[derive(Debug)]
pub struct Struct<'a> {
    pub visibility: Visibility,
    pub name: &'a str,
    pub fields: Vec<StructField<'a>>
}

#[derive(Debug)]
pub struct StructField<'a> {
    pub visibility: Visibility,
    pub name: &'a str,
    pub ty: Type<'a>,
}

#[derive(Debug)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug)]
pub enum Symbol<'b> {
    Function(Function<'b>),
    Struct(Struct<'b>),
}