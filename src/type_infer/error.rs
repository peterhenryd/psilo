use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::parser::ast::{BinaryOp, Name, Type, UnaryOp};

pub type TypeResult<'a, T> = Result<T, TypeError<'a>>;

pub enum TypeError<'a> {
    UnknownFunction(Name<'a>),
    IncompatibleUnaryOp(Type<'a>, UnaryOp),
    IncompatibleBinaryOp(Type<'a>, BinaryOp),
    ConflictingTypes(Type<'a>, Type<'a>),
    FunctionConflictingTypes(Type<'a>, Type<'a>)
}

impl Debug for TypeError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for TypeError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UnknownFunction(name) => {
                f.write_str("unknown function: ")?;
                f.write_str(name.0.join("::").as_str())
            }
            TypeError::IncompatibleUnaryOp(ty, op) => {
                Display::fmt(op, f)?;
                f.write_str(" cannot be used with the type ")?;
                Display::fmt(ty, f)
            }
            TypeError::IncompatibleBinaryOp(ty, op) =>{
                Display::fmt(op, f)?;
                f.write_str(" cannot be used with the type ")?;
                Display::fmt(ty, f)
            }
            TypeError::ConflictingTypes(original, new) => {
                f.write_str("conflicting types: ")?;
                Display::fmt(original, f)?;
                f.write_str(", ")?;
                Display::fmt(new, f)
            }
            TypeError::FunctionConflictingTypes(fun, given) => {
                f.write_str("expected type ")?;
                Display::fmt(fun, f)?;
                f.write_str(" but got ")?;
                Display::fmt(given, f)
            }
        }
    }
}

impl Error for TypeError<'_> {}