use std::fs;
use std::path::Path;
use crate::lexer::Lexer;
use crate::name_res::NameResolver;
use crate::parser::parse_module;
use crate::type_infer::TypeInferrer;

#[test]
fn test() {
    let file = fs::read_to_string(Path::new("./examples/hello_world.psi")).unwrap();
    let mut lexer = Lexer::new(file.as_str());

    let module = parse_module(&mut lexer, vec!["test"]);

    let mut module = match module {
        Ok(module) => module,
        Err(err) => panic!("{}", err),
    };

    let mut name_resolver = NameResolver::new();

    match name_resolver.resolve_module(&mut module) {
        Ok(()) => {}
        Err(err) => panic!("{}", err),
    };

    {
        let mut type_inferrer = TypeInferrer::new();

        let functions = type_inferrer.add_module(&mut module);

        match type_inferrer.infer_types(functions) {
            Ok(_) => {}
            Err(err) => {
                panic!("{}", err);
            }
        }
    }
}