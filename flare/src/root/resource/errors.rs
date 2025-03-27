use thiserror::Error;


use super::cst::SymbolType;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Error while typechecking")]
    TypecheckingError(#[from] TypecheckingError),
    #[error("Error while parsing")]
    ParseError(#[from] ParsingError),
    #[error("Error while lexing")]
    LexError(#[from] LexingError),

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
    UndefinedField { obj: String, field: String },

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
}


#[derive(Error, Debug)]
pub enum ParsingError {
    #[error(transparent)]
    ParseFail(ParsingFailError),
}

#[derive(Error, Debug)]
pub struct ParsingFailError {
    pub modulename: String,
    pub source: peg::error::ParseError<usize>,
}

impl std::fmt::Display for ParsingFailError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Failed to parse module '{}'", self.modulename))
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