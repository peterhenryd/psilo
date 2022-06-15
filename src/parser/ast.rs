#[derive(Debug)]
pub struct Module<'a> {
    pub name: &'a str,
    pub use_decls: Vec<LocalName<'a>>,
    pub symbols: Vec<Symbol<'a>>,
}

#[derive(Debug)]
pub enum Type<'a> {
    Sint(Option<u16>),
    Uint(Option<u16>),
    Float,
    F32,
    F64,
    Str,
    Char,
    Ref(&'a Type<'a>),
}

#[derive(Debug)]
pub enum SelfArgument {
    Default, Mut,
}

#[derive(Debug)]
pub struct MethodSignature<'a> {
    pub self_argument: Option<SelfArgument>,
    pub function: FunctionSignature<'a>,
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
    pub body: Block<'a>,
}

#[derive(Debug)]
pub struct Block<'a>(pub Vec<Stmt<'a>>);

#[derive(Debug)]
pub struct LocalName<'a>(pub Vec<&'a str>);

#[derive(Debug)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Return(Expr<'a>),
    When {
        matching: Expr<'a>,
        branches: Vec<WhenBranch<'a>>,
        default: Expr<'a>
    }
}

#[derive(Debug)]
pub struct WhenBranch<'a> {
    pub condition: Expr<'a>,
    pub result: Expr<'a>
}

#[derive(Debug)]
pub enum Expr<'a> {
    Unit,
    CallFunction {
        name: LocalName<'a>,
        arguments: Vec<Expr<'a>>
    },
    Block(Block<'a>),
    Literal(Literal<'a>),
    Binary {
        op: BinaryOp,
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr<'a>>,
    },
    Ctx(LocalName<'a>),
}

#[derive(Debug)]
pub enum BinaryOp {
    Eq,
    Neq,
    LogAnd,
    LogOr,
    And,
    Or,
    Xor,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Greater,
    Less,
    GreaterEq,
    LessEq,
    Shl,
    Shr,
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not
}

#[derive(Debug)]
pub enum Literal<'a> {
    Str(&'a str),
    Int(u128),
    Float(f64),
    Char(char),
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