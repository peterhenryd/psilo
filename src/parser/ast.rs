use crate::diagnostic::Location;

pub struct Module<'a> {
    pub name: &'a str,
    pub functions: Vec<Function<'a>>,
    pub structs: Vec<Struct<'a>>,
}

pub enum Type<'a> {

}

pub trait Symbol {
    fn visibility(&self) -> Visibility;

    fn name(&self) -> &str;
}

pub enum SelfArgument {
    Default, Mut,
}

pub struct MethodSignature<'a> {
    pub self_argument: Option<SelfArgument>,
    pub function: FunctionSignature<'a>,
}

pub struct FunctionSignature<'a> {
    pub arguments: Vec<FunctionArgument<'a>>,
    pub return_ty: Type<'a>
}

pub struct FunctionArgument<'a> {
    pub is_mut: bool,
    pub name: &'a str,
    pub ty: Type<'a>,
}

pub struct Function<'a> {
    pub visibility: Visibility,
    pub name: &'a str,
    pub signature: FunctionSignature<'a>,
    pub body: Block<'a>,
}

pub struct Block<'a>(pub Vec<Statement<'a>>);

pub struct LocalName<'a>(pub Vec<&'a str>);

pub enum Statement<'a> {
    Expr(Expr<'a>),
    Return(Expr<'a>)
}

pub enum Expr<'a> {
    CallFunction {
        name: LocalName<'a>,
        arguments: Vec<Expr<'a>>
    },
    Block(Block<'a>),
    Literal(Literal<'a>),
    Binary {
        op: BinaryOp,
        lhs: Expr<'a>,
        rhs: Expr<'a>
    },
    Unary {
        op: UnaryOp,
        expr: Expr<'a>,
    }
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

pub enum UnaryOp {
    Neg,
    Not
}

pub enum Literal<'a> {

}

pub struct Struct<'a> {
    pub visibility: Visibility,
    pub name: &'a str,
    pub fields: Vec<StructField<'a>>
}

pub struct StructField<'a> {
    pub visibility: Visibility,
    pub name: &'a str,
    pub ty: Type<'a>,
}

pub enum Visibility {
    Public,
    Private,
}