use std::iter::Peekable;
use std::str::FromStr;
use logos::{Logos, Lexer as LogosLexer, SpannedIter, Span};
use crate::lexer::id::TokenId;
use crate::parser::ast::Spanned;
use crate::parser::error::{ParseError, ParseResult};

pub mod id;

#[derive(Logos, Copy, Clone)]
pub enum Token<'a> {
    // KEYWORDS
    #[token("break")]    Break,
    #[token("continue")] Continue,
    #[token("else")]     Else,
    #[token("enum")]     Enum,
    #[token("extern")]   Extern,
    #[token("false")]    False,
    #[token("fn")]       Fn,
    #[token("for")]      For,
    #[token("if")]       If,
    #[token("impl")]     Impl,
    #[token("in")]       In,
    #[token("let")]      Let,
    #[token("when")]     When,
    #[token("mod")]      Mod,
    #[token("mut")]      Mut,
    #[token("pub")]      Pub,
    #[token("return")]   Return,
    #[token("self")]     SelfValue,
    #[token("Self")]     SelfType,
    #[token("struct")]   Struct,
    #[token("trait")]    Trait,
    #[token("true")]     True,
    #[token("unsafe")]   Unsafe,
    #[token("use")]      Use,
    #[token("where")]    Where,
    #[token("while")]    While,
    #[token("loop")]     Loop,
    #[token("async")]    Async,
    #[token("await")]    Await,
    #[token("dyn")]      Dyn,

    // SYMBOLS
    #[token("(")]  OpenParen,
    #[token(")")]  CloseParen,
    #[token("{")]  OpenBrace,
    #[token("}")]  CloseBrace,
    #[token("[")]  OpenBracket,
    #[token("]")]  CloseBracket,
    #[token("?")]  Question,
    #[token(".")]  Dot,
    #[token(",")]  Comma,
    #[token(";")]  Semicolon,
    #[token(":")]  Colon,
    #[token("::")] ColonColon,
    #[token("!=")] NotEq,
    #[token("==")] EqEq,
    #[token("+=")] AddEq,
    #[token("-=")] SubEq,
    #[token("*=")] MulEq,
    #[token("/=")] DivEq,
    #[token("%=")] RemEq,
    #[token("&=")] AndEq,
    #[token("|=")] OrEq,
    #[token("^=")] XorEq,
    #[token(">=")] GreaterEq,
    #[token("<=")] LessEq,
    #[token("!")]  Not,
    #[token("=")]  Eq,
    #[token("+")]  Add,
    #[token("-")]  Sub,
    #[token("*")]  Mul,
    #[token("/")]  Div,
    #[token("%")]  Rem,
    #[token("&")]  And,
    #[token("|")]  Or,
    #[token("^")]  Xor,
    #[token(">")]  Greater,
    #[token("<")]  Less,
    #[token("<<")] Shl,
    #[token(">>")] Shr,
    #[token("&&")] LogAnd,
    #[token("||")] LogOr,
    #[token("->")] Arrow,
    #[token("=>")] DubArrow,

    // LITERALS
    #[regex("0b[01]+", lex_binary_int)]
    #[regex("[0-9]+", lex_decimal_int)]
    #[regex("0o[1-7]+", lex_octal_int)]
    #[regex("0x[0-9A-F]+", lex_hexadecimal_int)] Int(u128),
    #[regex("[0-9]+.[0-9]+", lex_float)]         Float(f64),
    // TODO: this regex needs to be improve
    #[regex("'(.)'", lex_char)]                  Char(char),
    // TODO: this regex needs to be improved
    #[regex("\"([^\"]*)\"", lex_string)]         Str(&'a str),
    
    #[regex("[_]*[a-zA-Z][a-zA-Z0-9_]*")]             Ident(&'a str),

    #[error]
    #[regex(r"#(.*)\n", logos::skip)]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}


fn lex_binary_int<'a, 'b>(lexer: &'a mut LogosLexer<'b, Token<'b>>) -> u128 {
    let str: &str = lexer.slice();
    u128::from_str_radix(&str[2..str.len() - 1], 2).unwrap()
}

fn lex_decimal_int<'a, 'b>(lexer: &'a mut LogosLexer<'b, Token<'b>>) -> u128 {
    str::parse::<u128>(lexer.slice()).unwrap()
}

fn lex_octal_int<'a, 'b>(lexer: &'a mut LogosLexer<'b, Token<'b>>) -> u128 {
    let str: &str = lexer.slice();
    u128::from_str_radix(&str[2..str.len() - 1], 8).unwrap()
}

fn lex_hexadecimal_int<'a, 'b>(lexer: &'a mut LogosLexer<'b, Token<'b>>) -> u128 {
    let str: &str = lexer.slice();
    u128::from_str_radix(&str[2..str.len() - 1], 17).unwrap()
}

fn lex_float<'a, 'b>(lexer: &'a mut LogosLexer<'b, Token<'b>>) -> f64 {
    f64::from_str(lexer.slice()).unwrap()
}

fn lex_char<'a, 'b>(lexer: &'a mut LogosLexer<'b, Token<'b>>) -> char {
    let str: &str = lexer.slice();
    char::from_str(&str[1..str.len() - 2]).unwrap()
}

fn lex_string<'a, 'b>(lexer: &'a mut LogosLexer<'b, Token<'b>>) -> &'b str {
    let str: &str = lexer.slice();
    &str[1..str.len() - 1]
}

pub struct Lexer<'a> {
    pub iter: Peekable<SpannedIter<'a, Token<'a>>>,
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn new(source: &'a str) -> Self {
        Self { iter: LogosLexer::new(source).spanned().peekable() }
    }

    pub fn expect(&mut self, id: TokenId) -> ParseResult<'a, Span> {
        match self.iter.next() {
            Some((Token::Error, span)) => panic!("{} to {}", span.start, span.end),
            Some((token, span)) => if id.eq(&token) {
                Ok(span)
            } else {
                Err(ParseError::Expected(vec![id], Some(token)))
            }
            None => Err(ParseError::Expected(vec![id], None))
        }
    }

    pub fn expect_ident_spanned(&mut self) -> ParseResult<'a, Spanned<&'a str>> {
        match self.iter.next() {
            Some((Token::Ident(s), span)) => Ok(Spanned(span, s)),
            token => Err(
                ParseError::Expected(
                    vec![TokenId::Ident],
                    token.map(|(t, _)| t)
                )
            )
        }
    }

    pub fn expect_ident(&mut self) -> ParseResult<'a, &'a str> {
        match self.iter.next() {
            Some((Token::Ident(s), _)) => Ok(s),
            token => Err(
                ParseError::Expected(
                    vec![TokenId::Ident],
                    token.map(|(t, _)| t)
                )
            )
        }
    }

    pub fn maybe_expect(&mut self, id: TokenId) -> bool {
        match self.iter.peek() {
            Some((token, _)) => if id.eq(&token) {
                self.iter.next();
                true
            } else {
                false
            }
            None => false
        }
    }

    pub fn maybe_expect_span(&mut self, id: TokenId) -> Option<Span> {
        match self.iter.peek() {
            Some((token, span)) => if id.eq(&token) {
                let span = span.clone();
                self.iter.next();
                Some(span)
            } else {
                None
            }
            None => None
        }
    }

    pub fn peek_expect(&mut self, id: TokenId) -> bool {
        match self.iter.peek() {
            Some((token, _)) => id.eq(&token),
            None => false
        }
    }
}

