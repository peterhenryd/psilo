use std::iter::Peekable;
use std::str::FromStr;
use logos::{Logos, Lexer as LogosLexer};
use crate::diagnostic;

#[cfg(test)]
pub(crate) mod tests;

#[derive(Logos)]
pub enum Token {
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
    #[token("match")]    Match,
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
    #[token(".")]  Dot,
    #[token(",")]  Comma,
    #[token(";")]  Semicolon,
    #[token(":")]  Colon,
    #[token("==")] EqEq,
    #[token("+=")] AddEq,
    #[token("-=")] SubEq,
    #[token("*=")] MulEq,
    #[token("/=")] DivEq,
    #[token("%=")] RemEq,
    #[token("=")]  Eq,
    #[token("+")]  Add,
    #[token("-")]  Sub,
    #[token("*")]  Mul,
    #[token("%")]  Rem,
    #[token("&")]  And,
    #[token("|")]  Or,
    #[token("^")]  Xor,

    // LITERALS
    #[regex("0b[01]+", lex_binary_int)]
    #[regex("[0-9]+", lex_decimal_int)]
    #[regex("0o[1-7]+", lex_octal_int)]
    #[regex("0x[0-9A-F]+", lex_hexadecimal_int)] Int(u128),
    #[regex("[0-9]+.[0-9]+", lex_float)]         Float(f64),
    // TODO: this regex needs to be improved
    #[regex("'(.)'", lex_char)]                  Char(char),
    // TODO: this regex needs to be improved
    #[regex("\"([^\"]*)\"", lex_string)]         Str(String),
}

fn lex_binary_int(lexer: &mut LogosLexer<Token>) -> u128 {
    let str: &str = lexer.slice();
    u128::from_str_radix(&str[2..str.len() - 1], 2).unwrap()
}

fn lex_decimal_int(lexer: &mut LogosLexer<Token>) -> u128 {
    str::parse::<u128>(lexer.slice()).unwrap()
}

fn lex_octal_int(lexer: &mut LogosLexer<Token>) -> u128 {
    let str: &str = lexer.slice();
    u128::from_str_radix(&str[2..str.len() - 1], 8).unwrap()
}

fn lex_hexadecimal_int(lexer: &mut LogosLexer<Token>) -> u128 {
    let str: &str = lexer.slice();
    u128::from_str_radix(&str[2..str.len() - 1], 17).unwrap()
}

fn lex_float(lexer: &mut LogosLexer<Token>) -> f64 {
    f64::from_str(lexer.slice()).unwrap()
}

fn lex_char(lexer: &mut LogosLexer<Token>) -> char {
    let str: &str = lexer.slice();
    char::from_str(&str[1..str.len() - 2]).unwrap()
}

fn lex_string(lexer: &mut LogosLexer<Token>) -> String {
    let str: &str = lexer.slice();
    str[1..str.len() - 2].to_string()
}

pub struct Lexer<'a> {
    iter: Peekable<LogosLexer<'a, Token>>,
    reporter: &'a diagnostic::Reporter
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn new(source: &'a str, reporter: &'a diagnostic::Reporter) -> Self {
        Self { iter: LogosLexer::new(source).peekable(), reporter }
    }


}

