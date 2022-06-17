use std::collections::HashMap;
use crate::parser::ast::{Block, Expr, Function, Literal, Module, Name, Node, Spanned, Stmt, Struct, Symbol, Type, UnaryOp};
use crate::type_infer::error::{TypeError, TypeResult};

pub mod error;

pub struct TypeInferrer<'a> {
    pub structs: HashMap<Name<'a>, &'a Struct<'a>>,
    pub function_signatures: HashMap<Name<'a>, Type<'a>>
}

pub struct Binding<'a>(bool, Type<'a>);

impl<'a> TypeInferrer<'a> {
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
            function_signatures: HashMap::new()
        }
    }

    pub fn add_module(&mut self, module: &'a mut Module<'a>) -> HashMap<Name<'a>, &'a mut Function<'a>> {
        let mut functions = HashMap::new();

        for symbol in &mut module.symbols {
            let mut name = module.absolute_name.clone();

            match symbol {
                Symbol::Function(function) => {
                    name.push(function.name);

                    self.function_signatures.insert(Name(name.clone()), function.signature.return_ty.clone());

                    functions.insert(Name(name), function);
                }
                Symbol::Struct(structure) => {
                    name.push(structure.name);
                    self.structs.insert(Name(name), structure);
                }
            }
        }

        functions
    }

    pub fn infer_types(&self, functions: HashMap<Name<'a>, &'a mut Function<'a>>) -> TypeResult<()> {
        for (_, function) in functions {
            self.infer_function(function)?;
        }

        Ok(())
    }

    pub fn infer_function(&self, function: &mut Function<'a>) -> TypeResult<()> {
        let ty = self.infer_block(&mut function.body.1)?;

        if ty != function.signature.return_ty {
            Err(TypeError::FunctionConflictingTypes(function.signature.return_ty.clone(), ty))
        } else {
            Ok(())
        }
    }

    pub fn infer_block(&self, block: &mut Block<'a>) -> TypeResult<Type<'a>> {
        let mut bindings = HashMap::new();

        let mut ty: Option<Type> = None;

        for stmt in &mut block.0 {
            match self.infer_stmt(stmt, &mut bindings)? {
                None => {}
                Some(return_ty) => {

                    if let Some(ty) = ty {
                        if ty != return_ty {
                            return Err(TypeError::ConflictingTypes(ty, return_ty.clone()));
                        }
                    }

                    ty = Some(return_ty);
                }
            }
        }

        Ok(ty.unwrap_or_else(|| Type::Unit))
    }

    pub fn infer_stmt(&self, stmt: &mut Stmt<'a>, bindings: &mut HashMap<&'a str, Binding<'a>>) -> TypeResult<Option<Type<'a>>> {
        match stmt {
            Stmt::Expr(expr) => {
                self.infer_expr(expr, bindings)?;
                Ok(None)
            },
            Stmt::Return(expr) => {
                self.infer_expr(expr, bindings)?;

                Ok(expr.0.clone())
            },
            Stmt::Break(expr) => {
                self.infer_expr(expr, bindings)?;
                Ok(None)
            },
            Stmt::Continue => Ok(None),
            Stmt::Let { binding, is_mut, initial_value } => {
                self.infer_expr(initial_value, bindings)?;
                bindings.insert(binding, Binding(*is_mut, initial_value.0.as_ref().unwrap().clone()));

                Ok(None)
            },
            Stmt::While { condition, block } => {
                self.infer_expr(condition, bindings)?;
                self.infer_block(&mut block.1)?;

                Ok(None)
            }
        }
    }

    pub fn infer_expr(&self, expr: &mut Node<'a, Expr<'a>>, bindings: &mut HashMap<&'a str, Binding<'a>>) -> TypeResult<()> {
        expr.0 = match &mut expr.1.1 {
            Expr::Unit => Some(Type::Unit),
            Expr::CallFunction { name, .. } => {
                Some(self.find_function(&name.1)?)
            }
            Expr::Block(block) => Some(self.infer_block(block)?),
            Expr::Literal(literal) => match literal {
                Literal::Str(_) => Some(Type::Str),
                Literal::Int(_) => Some(Type::Uint(Some(32))), // TODO: fix
                Literal::Float(_) => Some(Type::F64), // TODO: fix
                Literal::Char(_) => Some(Type::Char),
                Literal::Bool(_) => Some(Type::Bool),
            }
            Expr::Binary { lhs, rhs, .. } => {
                self.infer_expr(lhs, bindings)?;
                self.infer_expr(rhs, bindings)?;

                lhs.0.clone() // TODO: fix
            }
            Expr::Unary { expr, op } => {
                self.infer_expr(expr, bindings)?;

                let ty: Type = expr.0.as_ref().unwrap().clone();

                Some(match op {
                    UnaryOp::Neg => match ty {
                        Type::Sint(_) | Type::Float | Type::F32 | Type::F64 => ty,
                        ty => return Err(TypeError::IncompatibleUnaryOp(ty, *op))
                    }
                    UnaryOp::Not => match ty {
                        Type::Bool => ty,
                        ty => return Err(TypeError::IncompatibleUnaryOp(ty, *op))
                    }
                    UnaryOp::Ref { is_mut } => Type::Ref { is_mut: *is_mut, ty: Box::new(ty) },
                    UnaryOp::Deref => match ty {
                        Type::Ref { ty, .. } => *ty,
                        ty => return Err(TypeError::IncompatibleUnaryOp(ty, *op))
                    }
                    UnaryOp::Try => panic!()
                })
            },
            Expr::When { matching, branches, default } => {
                self.infer_expr(matching, bindings)?;
                for branch in branches {
                    self.infer_expr(&mut branch.condition, bindings)?;
                    self.infer_expr(&mut branch.result, bindings)?;
                }
                self.infer_expr(default, bindings)?;

                default.0.clone() // TODO: fix
            }
            Expr::If { condition, then, default } => {
                self.infer_expr(condition, bindings)?;
                let ty = self.infer_block(&mut then.1)?;
                if let Some(ref mut default) = default {
                    self.infer_expr(default, bindings)?;

                    let default_ty = default.0.as_ref().unwrap().clone();
                    if ty != default_ty {
                        return Err(TypeError::ConflictingTypes(ty, default_ty))
                    }
                }

                Some(ty)
            }
            Expr::Loop(block) => Some(self.infer_block(block)?),
            Expr::Ctx(binding) => {
                Some(bindings.get(binding).unwrap().1.clone())
            }
            Expr::Field(expr, field) => {
                self.infer_expr(expr, bindings)?;

                if let Some(Type::Struct(Spanned(_, name))) = &mut expr.0 {
                    Some(self.structs.get(name)
                        .unwrap() // TODO: fix
                        .fields
                        .iter()
                        .find(|f| &f.name == field)
                        .unwrap()
                        .ty.clone())
                } else {
                    panic!("")
                }
            }
        };

        Ok(())
    }

    pub fn find_function(&self, name: &Name<'a>) -> TypeResult<Type<'a>> {
        self.function_signatures.get(name)
            .map(|ty| ty.clone())
            .ok_or_else(|| TypeError::UnknownFunction(name.clone()))
    }
}