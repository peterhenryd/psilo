use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::{Error, SimpleFiles};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

#[derive(Copy, Clone)]
pub struct Location {

}

pub struct Reporter {
    files: SimpleFiles<String, String>
}

impl Reporter {
    pub fn new() -> Self {
        Self {
            files: SimpleFiles::new()
        }
    }

    pub fn report(&mut self, file_id: usize,) {
        let diagnostic = Diagnostic::error()
            .with_message("`case` clauses have incompatible types")
            .with_code("E0308")
            .with_labels(vec![
                Label::primary(file_id, 328..331).with_message("expected `String`, found `Nat`"),
                Label::secondary(file_id, 211..331).with_message("`case` clauses have incompatible types"),
                Label::secondary(file_id, 258..268).with_message("this is found to be of type `String`"),
                Label::secondary(file_id, 284..290).with_message("this is found to be of type `String`"),
                Label::secondary(file_id, 306..312).with_message("this is found to be of type `String`"),
                Label::secondary(file_id, 186..192).with_message("expected type `String` found here"),
            ])
            .with_notes(vec![unindent::unindent(
                "
            expected type `String`
                found type `Nat`
        ",
            )]);


        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = term::Config::default();

        match term::emit(&mut writer.lock(), &config, &files, &diagnostic) {
            Ok(_) => {}
            Err(error) => match error {
                Error::FileMissing => {}
                Error::IndexTooLarge { .. } => {}
                Error::LineTooLarge { .. } => {}
                Error::ColumnTooLarge { .. } => {}
                Error::InvalidCharBoundary { .. } => {}
                Error::Io(_) => {}
            }
        }
    }
}