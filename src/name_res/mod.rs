//! The role of name resolution is to transform local names into absolute names, as well as to
//! ensure that names referenced in block contexts are valid.

use std::collections::HashMap;
use crate::name_res::error::{NameError, NameResult};
use crate::parser::ast::{Block, Expr, Function, Module, Name, Spanned, Stmt, Struct, Symbol, Type};

pub mod error;

pub struct Binding<'a> {
    pub name: &'a str,
    pub is_mut: bool,
}

pub type BlockScope<'a> = Vec<Binding<'a>>;

pub fn contains_binding(scope: &BlockScope, name: &str) -> bool {
    scope.iter()
        .find(|binding| binding.name == name)
        .is_some()
}

pub struct NameResolver<'a> {
    known_symbols: HashMap<&'a str, Name<'a>>,
}

impl<'a> NameResolver<'a> {
    pub fn new() -> Self {
        Self { known_symbols: HashMap::new() }
    }

    pub fn resolve_module(&'a mut self, module: &mut Module<'a>) -> NameResult<()> {
        self.integrate_used_symbols(module);

        for symbol in &mut module.symbols {
            match symbol {
                Symbol::Function(function) => self.resolve_function(function)?,
                Symbol::Struct(structure) => self.resolve_struct(structure)?
            }
        }

        Ok(())
    }

    pub fn resolve_function(&self, function: &mut Function<'a>) -> NameResult<()> {
        self.resolve_type(&mut function.signature.return_ty)?;

        let mut args = vec![];
        for arg in &mut function.signature.arguments {
            self.resolve_type(&mut arg.ty)?;
            args.push(Binding{ name: arg.name, is_mut: arg.is_mut });
        }

        let mut upper_scope = BlockScope::new();
        upper_scope.append(&mut args);

        self.resolve_block(&mut function.body.1, &mut upper_scope)?;

        Ok(())
    }

    pub fn resolve_struct(&self, structure: &mut Struct<'a>) -> NameResult<()> {
        for field in &mut structure.fields {
            self.resolve_type(&mut field.ty)?;
        }

        Ok(())
    }

    pub fn make_absolute(&self, name: &mut Spanned<Name<'a>>) {
        let absolute_hook = name.1.0.remove(name.1.0.len() - 1);

        let span = name.0.clone();

        let mut absolute_name = self.known_symbols[absolute_hook].0.clone();
        absolute_name.append(&mut name.1.0);

        *name = Spanned(span, Name(absolute_name));
    }

    pub fn resolve_type(&self, ty: &mut Type<'a>) -> NameResult<()> {
        if let Type::Struct(ref mut name) = ty {
            self.make_absolute(name);
        }

        Ok(())
    }

    pub fn resolve_block(&self, block: &mut Block<'a>, mut upper_scope: &mut BlockScope<'a>) -> NameResult<()> {
        let mut scope = BlockScope::new();
        scope.append(&mut upper_scope);

        for stmt in &mut block.0 {
            self.resolve_stmt(&mut scope, stmt)?;
        }

        Ok(())
    }

    pub fn resolve_stmt(&self, scope: &mut BlockScope<'a>, stmt: &mut Stmt<'a>) -> NameResult<()> {
        match stmt {
            Stmt::Expr(expr) => self.resolve_expr(&mut expr.1.1, scope)?,
            Stmt::Return(expr) => self.resolve_expr(&mut expr.1.1, scope)?,
            Stmt::Break(expr) => self.resolve_expr(&mut expr.1.1, scope)?,
            Stmt::Continue => {}
            Stmt::Let { binding: name, is_mut, initial_value } => {
                scope.push(Binding { name, is_mut: *is_mut });
                self.resolve_expr(&mut initial_value.1.1, scope)?;
            }
            Stmt::While { condition, block } => {
                self.resolve_expr(&mut condition.1.1, scope)?;
                self.resolve_block(&mut block.1, scope)?;
            }
        }

        Ok(())
    }

    pub fn resolve_expr(&self, expr: &mut Expr<'a>, scope: &mut BlockScope<'a>) -> NameResult<()> {
        match expr {
            Expr::Unit => Ok(()),
            Expr::CallFunction { arguments, name } => {
                for expr in arguments {
                    self.resolve_expr(&mut expr.1.1, scope)?;
                }
                self.make_absolute(name);
                Ok(())
            }
            Expr::Block(block) => self.resolve_block(block, scope),
            Expr::Literal(_) => Ok(()),
            Expr::Binary { lhs, rhs, .. } => {
                self.resolve_expr(&mut lhs.1.1, scope)?;
                self.resolve_expr(&mut rhs.1.1, scope)
            }
            Expr::Unary { expr, .. } => self.resolve_expr(&mut expr.1.1, scope),
            Expr::When { matching, branches, default } => {
                self.resolve_expr(&mut matching.1.1, scope)?;
                for branch in branches {
                    self.resolve_expr(&mut branch.condition.1.1, scope)?;
                    self.resolve_expr(&mut branch.result.1.1, scope)?;
                }
                self.resolve_expr(&mut default.1.1, scope)
            }
            Expr::If { condition, then, default } => {
                self.resolve_expr(&mut condition.1.1, scope)?;
                self.resolve_block(&mut then.1, scope)?;
                if let Some(expr) = default {
                    self.resolve_expr(&mut expr.1.1, scope)?;
                }
                Ok(())
            }
            Expr::Loop(block) => self.resolve_block(block, scope),
            Expr::Ctx(name) => {
                if !contains_binding(scope, name) {
                    Err(NameError::UnknownCtx(name))
                } else {
                    Ok(())
                }
            }
            Expr::Field(expr, _) => self.resolve_expr(&mut expr.1.1, scope),
        }
    }

    pub fn integrate_used_symbols(&mut self, module: &mut Module<'a>) {
        for absolute_name in &module.use_decls {
            let local_name = absolute_name.1.0.last().unwrap();

            self.known_symbols.insert(local_name, absolute_name.1.clone());
        }
    }
}