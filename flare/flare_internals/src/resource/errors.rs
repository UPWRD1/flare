use std::error::Error;

pub type CompResult<T> = Result<T, Box<dyn Error>>;
