
use std::{fmt::Display, io::{BufWriter, Cursor}};

use ariadne::{sources, Color, Label, Report, ReportKind};

use chumsky::span::SimpleSpan;
use thiserror::Error;

use crate::root::resource::cst::SymbolType;

pub type CompResult<T> = Result<T, CompilerErr>;


pub fn failure(
    msg: String,
    label: (String, SimpleSpan),
    extra_labels: impl IntoIterator<Item = (String, SimpleSpan)>,
    src: &str,
) -> ! {
    let fname = "example";
    Report::build(ReportKind::Error, (fname, label.1.into_range()))
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(&msg)
        .with_label(
            Label::new((fname, label.1.into_range()))
                .with_message(label.0)
                .with_color(Color::Red),
        )
        .with_labels(extra_labels.into_iter().map(|label2| {
            Label::new((fname, label2.1.into_range()))
                .with_message(label2.0)
                .with_color(Color::Yellow)
        }))
        .finish()
        .print(sources([(fname, src)]))
        .unwrap();
    std::process::exit(1)
}

pub trait ReportableError {
    fn report(&self);
}

#[derive(Debug, Error)]
pub enum CompilerErr {
    #[error(transparent)]
    Parse(#[from] ParseErr),

    #[error(transparent)]
    ErrorCollection(#[from] ErrorCollection),

    #[error(transparent)]
    Typecheck(#[from] TypecheckingError),
    
    #[error(transparent)]
    Environment(#[from] EnvironmentError),
    
    // Other error types...
    #[error(transparent)]
    Other(#[from] anyhow::Error), // Catch-all for unexpected errors
}

impl ReportableError for CompilerErr {
    fn report(&self) {
        match self {
            CompilerErr::Parse(error) => error.report(),
            CompilerErr::ErrorCollection(errors) => errors.report(),

            CompilerErr::Typecheck(error) => eprintln!("{}", error),
            CompilerErr::Environment(environment_error) => todo!(),
            CompilerErr::Other(error) => eprintln!("{}", error),
        }
    }
}

impl From<std::io::Error> for CompilerErr {
    fn from(value: std::io::Error) -> Self {
        Self::Other(value.into())
    }
}

#[derive(Error, Debug)]
pub enum TypecheckingError {
    #[error("Missing Main Function")]
    MissingMainFunction,
    #[error("Invalid function return type: expected {expected}, found {found}")]
    InvalidFunctionReturnType {
        expected: SymbolType,
        found: SymbolType,
    },
    #[error("Call to function '{name}' has an invalid type for argument '{arg}': expected {expected}, found {found}")]
    InvalidFunctionArgumentType {
        name: String,
        arg: String,
        expected: SymbolType,
        found: SymbolType,
    },

    #[error("Call to function '{name}' expects {expected} arguments, found {found}")]
    InvalidFunctionArgumentLen {
        name: String,
        expected: usize,
        found: usize,
    },

    #[error("The variable '{name}' is not defined")]
    UndefinedVariable { name: String },

    #[error("The function '{name}' is not defined")]
    UndefinedFunction { name: String },

    #[error("The type '{obj}' has no method '{name}'")]
    UndefinedMethod { obj: String, name: String },

    #[error("The type '{obj}' has no associated function '{name}'")]
    UndefinedAssoc { obj: String, name: String },

    #[error("The type '{name}' is not defined")]
    UndefinedType { name: String },

    #[error("The struct type '{obj}' has no field '{field}'")]
    UndefinedStructField { obj: String, field: String },

    #[error("The variant '{v}' of enum type '{t}' has no field '{field}'")]
    UndefinedVariantField { v: String, t: String, field: String },


    #[error("The struct type '{obj}' expects its field '{field}' to be of type '{expected}', found '{found}'")]
    InvalidStructInstanceField { obj: String, field: String, expected: SymbolType, found: SymbolType },

    #[error("Variable '{name}' has already been defined and is immutable")]
    NonMutableReassignment { name: String },

    #[error("Type '{name}' has already been defined")]
    RedefinedType {name: String},
    
    #[error("Type '{name}' has no defined methods")]
    NoMethods {name: String},

    #[error("The function '{the_func}' is not a method, and therefore cannot use 'self'. ")]
    SelfOutsideMethod {the_func: String},

    #[error("The type {l} and {r} are incompatible.")]
    IncompatibleTypes {l: SymbolType, r: SymbolType},

    #[error("Expected type {expected}, found {found}")]
    ExpectedType {expected: SymbolType, found: SymbolType},
}

#[derive(Error, Debug)]
pub struct ErrorCollection {
    pub errors: Vec<CompilerErr>
}

impl Display for ErrorCollection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for e in &self.errors {
            e.fmt(f)?
        }
        Ok(())
    }
}

impl From<Vec<CompilerErr>> for ErrorCollection {
    fn from(value: Vec<CompilerErr>) -> Self {
        Self {errors: value}
    }
}

impl ReportableError for ErrorCollection {
    fn report(&self) {
        for e in &self.errors {
            e.report();
        }
    }
}

#[derive(Error, Debug)]
pub struct ParseErr {
    pub filename: String,
    pub msg: String,
    pub label: (String, SimpleSpan),
    pub extra_labels: Vec<(String, SimpleSpan)>,
    pub src: String,
}

impl ParseErr {
    pub fn new(filename: impl Into<String>, msg: impl Into<String>, label: (String, SimpleSpan), extra_labels: Vec<(String, SimpleSpan)>, src: String) -> Self {
        Self { filename: filename.into(), msg: msg.into(), label, extra_labels, src }
    }

}

impl std::fmt::Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let fname = self.filename.clone();
    let mut buf = Cursor::new(vec![]);
    Report::build(ReportKind::Error, (fname.clone() ,self.label.1.into_range()))
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(&self.msg)
        .with_label(
            Label::new((fname.clone(), self.label.1.into_range()))
                .with_message(self.label.0.as_str())
                .with_color(Color::Red),
        )
        .with_labels(self.extra_labels.clone().into_iter().map(|label2| {
            Label::new((fname.clone(), label2.1.into_range()))
                .with_message(label2.0)
                .with_color(Color::Yellow)
        }))
        .finish()
        .write(sources([(fname, self.src.clone())]), &mut buf)
        .unwrap();
    write!(f, "{}", String::from_utf8_lossy(buf.into_inner().as_slice()))
    }
}

impl ReportableError for ParseErr {
    fn report(&self) {
        eprintln!("{}", self);
    }
}


#[derive(Error, Debug)]
pub enum EnvironmentError {
    #[error("File '{name}' could not be read")]
    BadFile {name: String},

}