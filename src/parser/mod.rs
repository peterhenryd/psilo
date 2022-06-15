use crate::lexer::{Lexer, Token};
use crate::lexer::id::TokenId;
use crate::parser::ast::{BinaryOp, Block, Expr, Function, FunctionArgument, FunctionSignature, Literal, LocalName, Module, Stmt, Struct, StructField, Symbol, Type, UnaryOp, Visibility, WhenBranch};
use crate::parser::error::{ParseError, ParseResult};

pub mod ast;
pub mod error;
#[cfg(test)]
pub mod test;

pub fn parse_module<'a, 'b>(lexer: &'a mut Lexer<'b>, name: &'b str) -> ParseResult<'b, Module<'b>> {
    let use_decls = parse_use_decls(lexer)?;
    let symbols = parse_symbols(lexer)?;

    Ok(Module {
        name,
        use_decls,
        symbols
    })
}

pub fn parse_use_decls<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Vec<LocalName<'b>>> {
    let mut use_decls = vec![];
    while let Some(Token::Use) = lexer.iter.peek() {
        use_decls.push(parse_use_decl(lexer)?);
    }

    Ok(use_decls)
}

pub fn parse_use_decl<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, LocalName<'b>> {
    lexer.iter.next();

    let local_name = parse_local_name(lexer)?;

    lexer.expect(TokenId::Semicolon)?;

    Ok(local_name)
}

pub fn parse_local_name<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, LocalName<'b>> {
    let mut name = vec![];

    loop {
        match lexer.iter.peek() {
            Some(Token::Ident(s)) => {
                name.push(*s);

                lexer.iter.next();

                match lexer.iter.peek() {
                    Some(Token::Dot) => {
                        lexer.iter.next();
                        continue
                    },
                    _ => break Ok(LocalName(name))
                    //Some(&token) => return Err(ParseError::Expected(vec![TokenId::Dot, TokenId::Semicolon], Some(token))),
                    // None => return Err(ParseError::Expected(vec![TokenId::Dot, TokenId::Semicolon], None))
                }
            }
            Some(token) => return Err(ParseError::Expected(vec![TokenId::Ident], Some(token.clone()))),
            None => return Err(ParseError::Expected(vec![TokenId::Ident], None))
        }
    }
}

pub fn parse_symbols<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Vec<Symbol<'b>>> {
    let mut symbols = vec![];

    loop {
        let is_public = lexer.maybe_expect(TokenId::Pub);
        let visibility = if is_public { Visibility::Public } else { Visibility::Private };

        let symbol = match lexer.iter.next() {
            Some(Token::Fn) => Symbol::Function(parse_function(lexer, visibility)?),
            Some(Token::Struct) => Symbol::Struct(parse_struct(lexer, visibility)?),
            Some(token) => return Err(
                ParseError::Expected(
                    vec![TokenId::Fn, TokenId::Struct],
                    Some(token)
                )
            ),
            None => if is_public {
                return Err(
                    ParseError::Expected(
                        vec![TokenId::Fn, TokenId::Struct],
                        None
                    )
                )
            } else {
                break;
            }
        };

        symbols.push(symbol);
    }

    Ok(symbols)
}

pub fn parse_function<'a, 'b>(lexer: &'a mut Lexer<'b>, visibility: Visibility) -> ParseResult<'b, Function<'b>> {
    let name = lexer.expect_ident()?;
    let signature = parse_function_signature(lexer)?;
    let body = parse_block(lexer)?;

    Ok(Function { visibility, name, signature, body })
}

pub fn parse_struct<'a, 'b>(lexer: &'a mut Lexer<'b>, visibility: Visibility) -> ParseResult<'b, Struct<'b>> {
    let name = lexer.expect_ident()?;
    let fields = parse_struct_fields(lexer)?;

    Ok(Struct { visibility, name, fields })
}

pub fn parse_function_signature<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, FunctionSignature<'b>> {
    let arguments = parse_function_arguments(lexer)?;

    lexer.expect(TokenId::Arrow)?;

    let return_ty = parse_ty(lexer)?;

    Ok(FunctionSignature { arguments, return_ty })
}

pub fn parse_function_arguments<'a, 'b>(lexer: &'a mut Lexer<'b>,) -> ParseResult<'b, Vec<FunctionArgument<'b>>> {
    lexer.expect(TokenId::OpenParen)?;

    let mut arguments = vec![];

    loop {
        if lexer.maybe_expect(TokenId::CloseParen) {
            break;
        }

        arguments.push(parse_function_argument(lexer)?);

        if lexer.maybe_expect(TokenId::Comma) {
            continue;
        }
    }

    Ok(arguments)
}

pub fn parse_function_argument<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, FunctionArgument<'b>> {
    let is_mut = lexer.maybe_expect(TokenId::Mut);
    let name = lexer.expect_ident()?;
    let ty = parse_ty(lexer)?;

    Ok(FunctionArgument { is_mut, name, ty })
}

pub fn parse_block<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Block<'b>> {
    lexer.expect(TokenId::OpenBrace)?;

    let mut stmts = vec![];

    loop {
        if lexer.maybe_expect(TokenId::CloseBrace) {
            break;
        }

        stmts.push(parse_stmt(lexer)?);
    }

    Ok(Block(stmts))
}

pub fn parse_struct_fields<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Vec<StructField<'b>>> {
    lexer.expect(TokenId::OpenBrace)?;

    let mut fields = vec![];

    loop {
        if lexer.maybe_expect(TokenId::CloseBrace) {
            break;
        }

        fields.push(parse_struct_field(lexer)?);

        if lexer.maybe_expect(TokenId::Comma) {
            continue;
        }
    }

    Ok(fields)
}

pub fn parse_struct_field<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, StructField<'b>> {
    let visibility = if lexer.maybe_expect(TokenId::Pub) { Visibility::Public } else { Visibility::Private };
    let name = lexer.expect_ident()?;
    let ty = parse_ty(lexer)?;

    Ok(StructField { visibility, name, ty })
}

pub fn parse_ty<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Type<'b>> {
    match lexer.iter.next() {
        Some(Token::Ident("sint")) => Ok(Type::Sint(None)),
        Some(Token::Ident("s8")) => Ok(Type::Sint(Some(8))),
        Some(Token::Ident("s16")) => Ok(Type::Sint(Some(16))),
        Some(Token::Ident("s32")) => Ok(Type::Sint(Some(32))),
        Some(Token::Ident("s64")) => Ok(Type::Sint(Some(64))),
        Some(Token::Ident("s128")) => Ok(Type::Sint(Some(128))),
        Some(Token::Ident("uint")) => Ok(Type::Uint(None)),
        Some(Token::Ident("u8")) => Ok(Type::Uint(Some(8))),
        Some(Token::Ident("u16")) => Ok(Type::Uint(Some(16))),
        Some(Token::Ident("u32")) => Ok(Type::Uint(Some(32))),
        Some(Token::Ident("u64")) => Ok(Type::Uint(Some(64))),
        Some(Token::Ident("u128")) => Ok(Type::Uint(Some(128))),
        Some(Token::Ident("float")) => Ok(Type::Float),
        Some(Token::Ident("f32")) => Ok(Type::F32),
        Some(Token::Ident("f64")) => Ok(Type::F64),
        Some(Token::Ident("str")) => Ok(Type::Str),
        Some(Token::Ident("char")) => Ok(Type::Char),
        token => Err(ParseError::ExpectedType(token))
    }
}

pub fn parse_stmt<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Stmt<'b>> {
    dbg!("hi");

    match lexer.iter.peek() {
        Some(Token::When) => parse_when_stmt(lexer),
        _ => {
            let expr = Stmt::Expr(parse_expr(lexer)?);
            lexer.expect(TokenId::Semicolon)?;

            Ok(expr)
        }
    }
}

pub fn parse_when_stmt<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Stmt<'b>> {
    lexer.iter.next();

    let matching = parse_expr(lexer)?;
    let (branches, default) = parse_when_branches(lexer)?;

    Ok(Stmt::When {
        matching, branches, default
    })
}

pub fn parse_when_branches<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, (Vec<WhenBranch<'b>>, Expr<'b>)> {
    lexer.expect(TokenId::OpenBrace)?;

    let mut when_branches = vec![];

    let default = loop {
        if lexer.maybe_expect(TokenId::Else) {
            lexer.expect(TokenId::DubArrow)?;

            let expr = parse_expr(lexer)?;
            lexer.expect(TokenId::CloseBrace)?;

            break expr;
        } else if lexer.maybe_expect(TokenId::CloseBrace) {
            break Expr::Unit;
        }

        when_branches.push(parse_when_branch(lexer)?);

        if lexer.maybe_expect(TokenId::Comma) {
            continue;
        }
    };

    Ok((when_branches, default))
}

pub fn parse_when_branch<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, WhenBranch<'b>> {
    let condition = parse_expr(lexer)?;
    lexer.expect(TokenId::DubArrow)?;
    let result = parse_expr(lexer)?;

    Ok(WhenBranch { condition, result })
}

pub fn parse_expr<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Expr<'b>> {
    let lhs = parse_expr_2(lexer)?;

    match lexer.iter.peek() {
        Some(Token::LogOr) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::LogOr,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_2(lexer)?)
            })
        },
        _ => Ok(lhs)
    }
}

pub fn parse_expr_2<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Expr<'b>> {
    let lhs = parse_expr_3(lexer)?;

    match lexer.iter.peek() {
        Some(Token::LogAnd) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::LogAnd,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_3(lexer)?)
            })
        },
        _ => Ok(lhs)
    }
}

pub fn parse_expr_3<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Expr<'b>> {
    let lhs = parse_expr_4(lexer)?;

    match lexer.iter.peek() {
        Some(Token::Or) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Or,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_4(lexer)?)
            })
        },
        _ => Ok(lhs)
    }
}

pub fn parse_expr_4<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Expr<'b>> {
    let lhs = parse_expr_5(lexer)?;

    match lexer.iter.peek() {
        Some(Token::Xor) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Xor,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_5(lexer)?)
            })
        },
        _ => Ok(lhs)
    }
}

pub fn parse_expr_5<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Expr<'b>> {
    let lhs = parse_expr_6(lexer)?;

    match lexer.iter.peek() {
        Some(Token::And) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::And,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_6(lexer)?)
            })
        },
        _ => Ok(lhs)
    }
}

pub fn parse_expr_6<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Expr<'b>> {
    let lhs = parse_expr_7(lexer)?;

    match lexer.iter.peek() {
        Some(Token::EqEq) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Eq,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_7(lexer)?)
            })
        },
        Some(Token::NotEq) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Neq,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_7(lexer)?)
            })
        },
        _ => Ok(lhs)
    }
}

pub fn parse_expr_7<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Expr<'b>> {
    let lhs = parse_expr_8(lexer)?;

    match lexer.iter.peek() {
        Some(Token::Less) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Less,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_8(lexer)?)
            })
        },
        Some(Token::LessEq) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::LessEq,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_8(lexer)?)
            })
        },
        Some(Token::Greater) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Greater,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_8(lexer)?)
            })
        },
        Some(Token::GreaterEq) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::GreaterEq,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_8(lexer)?)
            })
        },
        _ => Ok(lhs)
    }
}

pub fn parse_expr_8<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Expr<'b>> {
    let lhs = parse_expr_9(lexer)?;

    match lexer.iter.peek() {
        Some(Token::Shl) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Shl,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_9(lexer)?)
            })
        },
        Some(Token::Shr) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Shr,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_9(lexer)?)
            })
        },
        _ => Ok(lhs)
    }
}

pub fn parse_expr_9<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Expr<'b>> {
    let lhs = parse_expr_10(lexer)?;

    match lexer.iter.peek() {
        Some(Token::Add) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Add,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_10(lexer)?)
            })
        },
        Some(Token::Sub) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Sub,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_10(lexer)?)
            })
        },
        _ => Ok(lhs)
    }
}

pub fn parse_expr_10<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Expr<'b>> {
    let lhs = parse_expr_11(lexer)?;

    match lexer.iter.peek() {
        Some(Token::Mul) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Mul,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_11(lexer)?)
            })
        },
        Some(Token::Div) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Div,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_11(lexer)?)
            })
        },
        Some(Token::Rem) => {
            lexer.iter.next();
            Ok(Expr::Binary {
                op: BinaryOp::Rem,
                lhs: Box::new(lhs),
                rhs: Box::new(parse_expr_11(lexer)?)
            })
        },
        _ => Ok(lhs)
    }
}

pub fn parse_expr_11<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Expr<'b>> {
    match lexer.iter.peek() {
        Some(Token::Not) => {
            lexer.iter.next();
            Ok(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(parse_expr(lexer)?)
            })
        },
        Some(Token::Sub) => {
            lexer.iter.next();
            Ok(Expr::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(parse_expr(lexer)?)
            })
        },
        Some(Token::Ident(_)) => {
            let name = parse_local_name(lexer)?;

            if lexer.maybe_expect(TokenId::OpenParen) {
                let mut arguments = vec![];

                loop {
                    if lexer.maybe_expect(TokenId::CloseParen) {
                        break;
                    }

                    arguments.push(parse_expr(lexer)?);

                    if lexer.maybe_expect(TokenId::Comma) {
                        continue;
                    }
                }

                Ok(Expr::CallFunction { name, arguments })
            } else {
                Ok(Expr::Ctx(name))
            }
        }
        Some(Token::Str(str)) => {
            let str = *str;
            lexer.iter.next();
            Ok(Expr::Literal(Literal::Str(str)))
        },
        Some(Token::Int(int)) => {
            let int = *int;
            lexer.iter.next();
            Ok(Expr::Literal(Literal::Int(int)))
        },
        Some(Token::Float(float)) => {
            let float = *float;
            lexer.iter.next();
            Ok(Expr::Literal(Literal::Float(float)))
        },
        Some(Token::Char(char)) => {
            let char = *char;
            lexer.iter.next();
            Ok(Expr::Literal(Literal::Char(char)))
        },
        token => Err(ParseError::ExpectedExpr(token.map(|t| t.clone())))
    }
}
