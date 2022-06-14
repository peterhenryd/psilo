use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

pub enum ParseResult {
    Ok()
}

pub enum ParseError<'a> {

}

impl Debug for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for ParseError {}