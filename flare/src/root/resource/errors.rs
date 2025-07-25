use std::{fmt::Display, io::BufWriter};

use thiserror::Error;

pub type CompResult<T> = Result<T, CompilerErr>;

use super::cst::SymbolType;

pub trait ReportableError {
    fn report(&self);
}

#[derive(Debug, Error)]
pub enum CompilerErr {
    #[error(transparent)]
    Parse(#[from] ParseErr),

    #[error(transparent)]
    ParseCollection(#[from] ParseErrorCollection),

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
            CompilerErr::ParseCollection(errors) => errors.report(),

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
pub struct ParseErrorCollection {
    errors: Vec<ParseErr>
}

impl Display for ParseErrorCollection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for e in &self.errors {
            e.fmt(f)?
        }
        Ok(())
    }
}

impl From<Vec<ParseErr>> for ParseErrorCollection {
    fn from(value: Vec<ParseErr>) -> Self {
        Self {errors: value}
    }
}

impl ReportableError for ParseErrorCollection {
    fn report(&self) {
        for e in &self.errors {
            e.report();
        }
    }
}

#[derive(Error, Debug)]
pub struct ParseErr {
    pub msg: String,
    pub source: Option<anyhow::Error>,
}

impl ParseErr {
    pub fn new(msg: impl Into<String>, source: Option<anyhow::Error> ) -> Self {
        Self {msg: msg.into(), source}
    }

    // pub fn new_from_lrpar_err(e: LexParseError<u32, DefaultLexerTypes>, filename: impl Into<String> + Clone, src_string: &str,) -> Self {
    //     use ariadne::{Color, ColorGenerator, Label, Report, ReportKind, Source};
    //     let mut colors = ColorGenerator::new();
    
    //     // Generate & choose some colours for each of our elements
    //     let a = colors.next();
    //     let out = Color::Fixed(81);
    //     let mut report = BufWriter::new(vec![]);

    //     if let LexParseError::ParseError(pe) = e {
    //             //println!("{}", e.pp(&lexer, &crate::flare_y::token_epp));
                
    //             let lexeme_span = pe.lexeme().span();
    //             let suggestion = match &pe.repairs().first().unwrap().first().unwrap() {
    //                 ParseRepair::Insert(tidx) => format!("insert '{}'", crate::flare_y::token_epp(*tidx).unwrap()),
    //                 ParseRepair::Delete(l) => format!("delete the offending token"),
    //                 ParseRepair::Shift(l) => todo!(),
    //             };
    //             let ariadne_span: (String, std::ops::Range<usize>) = (filename.clone().into(), lexeme_span.start()..lexeme_span.end());

    //             Report::build(ReportKind::Error, ariadne_span.clone()).with_message("Parsing Error")
    //             .with_label(
    //                 Label::new(ariadne_span.clone())
    //                     .with_message(format!("Error occured here"))
    //                     .with_color(a),
    //             )
    //             // .with_label(
    //             //     Label::new((ariadne_span.clone().0, ariadne_span.clone().1.start - 5..ariadne_span.clone().1.end))
    //             //         .with_message("In this clause")
    //             // )
    //             .with_help(suggestion).finish().write_for_stdout((filename.into(), Source::from(src_string)), &mut report).unwrap();
    //         let report_string = String::from_utf8_lossy(&report.into_inner().unwrap()).to_string();
    //         //dbg!(&report_string);
    //             Self::new(report_string, Some(Box::new(pe)))
    //     } else {
    //         unreachable!()
    //     }

    // }
}

impl std::fmt::Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}", self.msg))
    }
}

impl ReportableError for ParseErr {
    fn report(&self) {
        eprintln!("{}", self);
    }
}

#[derive(Error, Debug, Clone, PartialEq, Default)]
pub enum LexingError {
    #[error("")]
    #[default]
    LexError
}


#[derive(Error, Debug)]
pub enum EnvironmentError {
    #[error("File '{name}' could not be read")]
    BadFile {name: String},

}