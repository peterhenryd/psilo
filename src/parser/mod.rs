use logos::Span;
use crate::lexer::{Lexer, Token};
use crate::lexer::id::TokenId;
use crate::parser::ast::{BinaryOp, Block, Expr, Function, FunctionArgument, FunctionSignature, Literal, Name, Module, Stmt, Struct, StructField, Symbol, Type, UnaryOp, Visibility, WhenBranch, Node, Spanned};
use crate::parser::error::{ParseError, ParseResult};

pub mod ast;
pub mod error;
#[cfg(test)]
pub mod test;

pub fn parse_module<'a, 'b>(lexer: &'a mut Lexer<'b>, name: Vec<&'b str>) -> ParseResult<'b, Module<'b>> {
    let use_decls = parse_use_decls(lexer)?;
    let symbols = parse_symbols(lexer)?;

    Ok(Module { absolute_name: name, use_decls, symbols })
}

pub fn parse_use_decls<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Vec<Spanned<Name<'b>>>> {
    let mut use_decls = vec![];

    while lexer.maybe_expect(TokenId::Use) {
        use_decls.push(parse_use_decl(lexer)?);
    }

    Ok(use_decls)
}

pub fn parse_use_decl<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Spanned<Name<'b>>> {
    let local_name = parse_local_name(lexer)?;

    lexer.expect(TokenId::Semicolon)?;

    Ok(local_name)
}

pub fn parse_local_name<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Spanned<Name<'b>>> {
    let mut name = vec![];

    let start = lexer.iter.peek().unwrap().1.start;

    loop {
        match lexer.iter.peek() {
            Some((Token::Ident(s), _)) => {
                name.push(*s);

                lexer.iter.next();

                match lexer.iter.peek() {
                    Some((Token::ColonColon, _)) => {
                        lexer.iter.next();
                        continue
                    },
                    Some((_, span)) => break Ok(Spanned(start..span.end, Name(name))),
                    _ => panic!("parse_local_name"),
                    //Some(&token) => return Err(ParseError::Expected(vec![TokenId::Dot, TokenId::Semicolon], Some(token))),
                    // None => return Err(ParseError::Expected(vec![TokenId::Dot, TokenId::Semicolon], None))
                }
            }
            Some((token, _)) => return Err(ParseError::Expected(vec![TokenId::Ident], Some(token.clone()))),
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
            Some((Token::Fn, _)) => Symbol::Function(parse_function(lexer, visibility)?),
            Some((Token::Struct, _)) => Symbol::Struct(parse_struct(lexer, visibility)?),
            Some((token, _)) => return Err(
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

    let return_ty = if lexer.maybe_expect(TokenId::Arrow) {
        parse_ty(lexer)?
    } else {
        Type::Unit
    };

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

pub fn parse_block<'a, 'b>(lexer: &'a
mut Lexer<'b>) -> ParseResult<'b, Spanned<Block<'b>>> {
    let start = lexer.expect(TokenId::OpenBrace)?.start;

    let mut stmts = vec![];

    let end = loop {
        if let Some(span) = lexer.maybe_expect_span(TokenId::CloseBrace) {
            break span.end;
        }

        stmts.push(parse_stmt(lexer)?);
    };

    Ok(Spanned(start..end, Block(stmts)))
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
    match lexer.iter.peek() {
        Some((Token::And, _)) => {
            lexer.iter.next();
            let is_mut = lexer.maybe_expect(TokenId::Mut);
            let ty = Box::new(parse_ty(lexer)?);

            Ok(Type::Ref { is_mut, ty })
        },
        Some((Token::Ident("sint"), _)) => {
            lexer.iter.next();
            Ok(Type::Sint(None))
        },
        Some((Token::Ident("s8"), _)) => {
            lexer.iter.next();
            Ok(Type::Sint(Some(8)))
        },
        Some((Token::Ident("s16"), _)) => {
            lexer.iter.next();
            Ok(Type::Sint(Some(16)))
        },
        Some((Token::Ident("s32"), _)) => {
            lexer.iter.next();
            Ok(Type::Sint(Some(32)))
        },
        Some((Token::Ident("s64"), _)) => {
            lexer.iter.next();
            Ok(Type::Sint(Some(64)))
        },
        Some((Token::Ident("s128"), _)) => {
            lexer.iter.next();
            Ok(Type::Sint(Some(128)))
        },
        Some((Token::Ident("uint"), _)) => {
            lexer.iter.next();
            Ok(Type::Uint(None))
        },
        Some((Token::Ident("u8"), _)) => {
            lexer.iter.next();
            Ok(Type::Uint(Some(8)))
        },
        Some((Token::Ident("u16"), _)) => {
            lexer.iter.next();
            Ok(Type::Uint(Some(16)))
        },
        Some((Token::Ident("u32"), _)) => {
            lexer.iter.next();
            Ok(Type::Uint(Some(32)))
        },
        Some((Token::Ident("u64"), _)) => {
            lexer.iter.next();
            Ok(Type::Uint(Some(64)))
        },
        Some((Token::Ident("u128"), _)) => {
            lexer.iter.next();
            Ok(Type::Uint(Some(128)))
        },
        Some((Token::Ident("float"), _)) => {
            lexer.iter.next();
            Ok(Type::Float)
        },
        Some((Token::Ident("f32"), _)) => {
            lexer.iter.next();
            Ok(Type::F32)
        },
        Some((Token::Ident("f64"), _)) => {
            lexer.iter.next();
            Ok(Type::F64)
        },
        Some((Token::Ident("str"), _)) => {
            lexer.iter.next();
            Ok(Type::Str)
        },
        Some((Token::Ident("char"), _)) => {
            lexer.iter.next();
            Ok(Type::Char)
        },
        Some((Token::Ident(_), _)) => {
            Ok(Type::Struct(parse_local_name(lexer)?))
        },
        token => Err(ParseError::ExpectedType(token.map(|(t, _)| t.clone())))
    }
}

pub fn parse_stmt<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Stmt<'b>> {
    match lexer.iter.peek() {
        Some((Token::Let, _)) => parse_let_stmt(lexer),
        Some((Token::While, _)) => parse_while_stmt(lexer),
        Some((Token::Continue, _)) => {
            lexer.iter.next();
            lexer.expect(TokenId::Semicolon)?;
            Ok(Stmt::Continue)
        },
        Some((Token::Break, Span { start, .. })) => {
            let start = *start;
            lexer.iter.next();
            let expr = parse_expr(lexer)?;
            lexer.expect(TokenId::Semicolon)?;
            Ok(Stmt::Break(Node::new(start..expr.1.0.end, expr.0, expr.1.1)))
        },
        Some((Token::Return, _)) => {
            lexer.iter.next();
            let expr = parse_expr(lexer)?;
            lexer.expect(TokenId::Semicolon)?;
            Ok(Stmt::Return(expr))
        },
        _ => {
            let expr = parse_expr(lexer)?;

            if lexer.peek_expect(TokenId::CloseBrace) {
                Ok(Stmt::Return(expr))
            } else {
                match expr.1.1 {
                    Expr::When { .. } | Expr::If { .. } => {},
                    _ => {
                        lexer.expect(TokenId::Semicolon)?;
                    }
                }

                Ok(Stmt::Expr(expr))
            }
        }
    }
}

pub fn parse_while_stmt<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Stmt<'b>> {
    lexer.iter.next();

    let condition = parse_expr(lexer)?;
    let block = parse_block(lexer)?;

    Ok(Stmt::While { condition, block })
}

pub fn parse_let_stmt<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Stmt<'b>> {
    lexer.iter.next();
    let is_mut = lexer.maybe_expect(TokenId::Mut);
    let binding = lexer.expect_ident()?;
    lexer.expect(TokenId::Eq)?;
    let initial_value = parse_expr(lexer)?;
    lexer.expect(TokenId::Semicolon)?;

    Ok(Stmt::Let { binding, is_mut, initial_value })
}

pub fn parse_when_branches<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, (usize, Vec<WhenBranch<'b>>, Node<'b, Expr<'b>>)> {
    lexer.expect(TokenId::OpenBrace)?;

    let mut when_branches = vec![];

    let (default, end) = loop {
        if lexer.maybe_expect(TokenId::Else) {
            lexer.expect(TokenId::DubArrow)?;

            let expr = parse_expr(lexer)?;
            lexer.expect(TokenId::CloseBrace)?;

            let end = expr.1.0.end;

            break (expr, end);
        } else if let Some(span) = lexer.maybe_expect_span(TokenId::CloseBrace) {
            break (Node::new(0..0, Some(Type::Unit), Expr::Unit), span.end);
        }

        when_branches.push(parse_when_branch(lexer)?);

        if lexer.maybe_expect(TokenId::Comma) {
            continue;
        }
    };

    Ok((end, when_branches, default))
}

pub fn parse_when_branch<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, WhenBranch<'b>> {
    let condition = parse_expr(lexer)?;
    lexer.expect(TokenId::DubArrow)?;
    let result = parse_expr(lexer)?;

    Ok(WhenBranch { condition, result })
}

pub fn parse_expr<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let expr = parse_expr_2(lexer)?;

    match lexer.iter.peek() {
        Some((Token::LogOr, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::LogOr,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        _ => Ok(expr)
    }
}

pub fn parse_expr_2<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let expr = parse_expr_3(lexer)?;

    match lexer.iter.peek() {
        Some((Token::LogAnd, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::LogAnd,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        _ => Ok(expr)
    }
}

pub fn parse_expr_3<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let expr = parse_expr_4(lexer)?;

    match lexer.iter.peek() {
        Some((Token::Or, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Or,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        _ => Ok(expr)
    }
}

pub fn parse_expr_4<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let expr = parse_expr_5(lexer)?;

    match lexer.iter.peek() {
        Some((Token::Xor, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Xor,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        _ => Ok(expr)
    }
}

pub fn parse_expr_5<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let expr = parse_expr_6(lexer)?;

    match lexer.iter.peek() {
        Some((Token::And, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::And,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        _ => Ok(expr)
    }
}

pub fn parse_expr_6<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let expr = parse_expr_7(lexer)?;

    match lexer.iter.peek() {
        Some((Token::EqEq, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Eq,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        Some((Token::NotEq, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Neq,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        _ => Ok(expr)
    }
}

pub fn parse_expr_7<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let expr = parse_expr_8(lexer)?;

    match lexer.iter.peek() {
        Some((Token::Less, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Less,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        Some((Token::LessEq, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::LessEq,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        Some((Token::Greater, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Greater,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        Some((Token::GreaterEq, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::GreaterEq,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        _ => Ok(expr)
    }
}

pub fn parse_expr_8<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let expr = parse_expr_9(lexer)?;

    match lexer.iter.peek() {
        Some((Token::Shl, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Shl,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        Some((Token::Shr, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Shr,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        _ => Ok(expr)
    }
}

pub fn parse_expr_9<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let expr = parse_expr_10(lexer)?;

    match lexer.iter.peek() {
        Some((Token::Add, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Add,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        Some((Token::Sub, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Sub,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        _ => Ok(expr)
    }
}

pub fn parse_expr_10<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let expr = parse_expr_11(lexer)?;

    match lexer.iter.peek() {
        Some((Token::Mul, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Mul,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        Some((Token::Div, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Div,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        Some((Token::Rem, _)) => {
            lexer.iter.next();
            let rhs = parse_expr(lexer)?;
            Ok(Node::new(expr.1.0.start..rhs.1.0.end, None, Expr::Binary {
                op: BinaryOp::Rem,
                lhs: Box::new(expr),
                rhs: Box::new(rhs)
            }))
        },
        _ => Ok(expr)
    }
}

pub fn parse_expr_11<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let (op, start) = match lexer.iter.peek() {
        Some((Token::Sub, span)) => {
            let start = span.start;
            lexer.iter.next();
            (UnaryOp::Neg, start)
        },
        Some((Token::Not, span)) => {
            let start = span.start;
            lexer.iter.next();
            (UnaryOp::Not, start)
        },
        Some((Token::And, span)) => {
            let start = span.start;
            lexer.iter.next();
            let is_mut = lexer.maybe_expect(TokenId::Mut);
            (UnaryOp::Ref { is_mut }, start)
        },
        Some((Token::Mul, span)) => {
            let start = span.start;
            lexer.iter.next();
            (UnaryOp::Deref, start)
        },
        _ => return parse_expr_12(lexer)
    };
    let expr = parse_expr_12(lexer)?;

    Ok(Node::new(start..expr.1.0.end, None, Expr::Unary { op, expr: Box::new(expr) }))
}

pub fn parse_expr_12<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let expr = match lexer.iter.peek() {
        Some((Token::OpenParen, span)) => {
            let start = span.start;
            lexer.iter.next();
            let mut expr = parse_expr(lexer)?;
            let end = lexer.expect(TokenId::CloseParen)?.end;
            expr.1.0 = start..end;
            Ok(expr)
        }
        Some((Token::OpenBrace, _)) => {
            let block = parse_block(lexer)?;
            Ok(Node::new(block.0, None, Expr::Block(block.1)))
        },
        Some((Token::When, _)) => parse_when_expr(lexer),
        Some((Token::If, _)) => parse_if_expr(lexer),
        Some((Token::Loop, _)) => {
            let block = parse_block(lexer)?;
            Ok(Node::new(block.0, None, Expr::Loop(block.1)))
        },
        Some((Token::Not, span)) => {
            let start = span.start;
            lexer.iter.next();
            let expr = parse_expr(lexer)?;
            Ok(Node::new(start..expr.1.0.end, None, Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(parse_expr(lexer)?)
            }))
        },
        Some((Token::Sub, span)) => {
            let start = span.start;
            lexer.iter.next();
            let expr = parse_expr(lexer)?;
            Ok(Node::new(start..expr.1.0.end, None, Expr::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(parse_expr(lexer)?)
            }))
        },
        Some((Token::True, span)) => {
            let span = span.clone();
            lexer.iter.next();
            Ok(Node::new(span, None, Expr::Literal(Literal::Bool(true))))
        },
        Some((Token::False, span)) => {
            let span = span.clone();
            lexer.iter.next();
            Ok(Node::new(span, None, Expr::Literal(Literal::Bool(false))))
        },
            Some((Token::Ident(_), _)) => parse_ident_expr(lexer),
        Some((Token::Str(str), span)) => {
            let str = *str;
            let span = span.clone();
            lexer.iter.next();
            Ok(Node::new(span, Some(Type::Str), Expr::Literal(Literal::Str(str))))
        },
        Some((Token::Int(int), span)) => {
            let int = *int;
            let span = span.clone();
            lexer.iter.next();
            Ok(Node::new(span, None, Expr::Literal(Literal::Int(int))))
        },
        Some((Token::Float(float), span)) => {
            let float = *float;
            let span = span.clone();
            lexer.iter.next();
            Ok(Node::new(span, None, Expr::Literal(Literal::Float(float))))
        },
        Some((Token::Char(char), span)) => {
            let char = *char;
            let span = span.clone();
            lexer.iter.next();
            Ok(Node::new(span, Some(Type::Char), Expr::Literal(Literal::Char(char))))
        },
        token => Err(ParseError::ExpectedExpr(token.map(|(t, _)| t.clone())))
    }?;

    parse_expr_13(lexer, expr)
}

pub fn parse_expr_13<'a, 'b>(lexer: &'a mut Lexer<'b>, expr: Node<'b, Expr<'b>>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    match lexer.iter.peek() {
        Some((Token::Question, span)) => {
            let end = span.end;
            lexer.iter.next();
            parse_expr_13(lexer, Node::new(expr.1.0.start..end, None, Expr::Unary {
                op: UnaryOp::Try,
                expr: Box::new(expr)
            }))
        }
        Some((Token::Dot, _)) => {
            lexer.iter.next();

            let field = lexer.expect_ident_spanned()?;
            parse_expr_13(lexer, Node::new(
                expr.1.0.start..field.0.end, None,
                Expr::Field(Box::new(expr), field.1)
            ))
        }
        _ => Ok(expr)
    }
}

pub fn parse_when_expr<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let start = lexer.iter.next().unwrap().1.start;

    let matching = Box::new(parse_expr(lexer)?);
    let (end, branches, default) = parse_when_branches(lexer)?;

    Ok(Node::new(start..end, None, Expr::When {
        matching, branches, default: Box::new(default)
    }))
}

pub fn parse_ident_expr<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let name = parse_local_name(lexer)?;

    if let Some(Span { start, .. }) = lexer.maybe_expect_span(TokenId::OpenParen) {
        let mut arguments = vec![];

        let end = loop {
            if let Some(span) = lexer.maybe_expect_span(TokenId::CloseParen) {
                break span.end;
            }

            arguments.push(parse_expr(lexer)?);

            if lexer.maybe_expect(TokenId::Comma) {
                continue;
            }
        };

        Ok(Node::new(start..end, None, Expr::CallFunction { name, arguments }))
    } else if name.1.0.len() == 1 {
        Ok(Node::new(name.0, None, Expr::Ctx(&name.1.0[0])))
    } else {
        todo!()
    }
}

pub fn parse_if_expr<'a, 'b>(lexer: &'a mut Lexer<'b>) -> ParseResult<'b, Node<'b, Expr<'b>>> {
    let start = lexer.iter.next().unwrap().1.start;

    let condition = Box::new(parse_expr(lexer)?);
    let then = parse_block(lexer)?;

    let mut end = then.0.end;

    let default = if lexer.maybe_expect(TokenId::Else) {
        Some(Box::new(if lexer.peek_expect(TokenId::If) {
            let node = parse_if_expr(lexer)?;
            end = node.1.0.end;
            node
        } else {
            let block = parse_block(lexer)?;
            Node::new(block.0, None, Expr::Block(block.1))
        }))
    } else {
        None
    };

    Ok(Node::new(start..end, None, Expr::If { condition, then, default }))
}