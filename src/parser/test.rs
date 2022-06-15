use std::fs;
use std::path::Path;
use crate::lexer::Lexer;
use crate::parser::parse_module;

#[test]
fn test() {
    let file = fs::read_to_string(Path::new("./examples/fib.psi")).unwrap();
    let mut lexer = Lexer::new(file.as_str());
    let module = parse_module(&mut lexer, "test");

    match module {
        Ok(module) => dbg!(module),
        Err(err) => panic!("{}", err),
    };
}