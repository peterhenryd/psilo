use crate::diagnostic;
use crate::lexer::Lexer;
use crate::parser::ast::Module;
use crate::parser::error::{ParseError, ParseResult};

mod ast;
mod error;

pub fn parse_module<'a>(lexer: &mut Lexer) -> ParseResult<Module<'a>, ParseError> {

}