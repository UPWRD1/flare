pub mod ast;
pub mod entry;
pub mod files;
pub mod mir;
pub mod quantifier;
pub mod types;

use chumsky::span::SimpleSpan;

use files::FileID;
use serde::{Deserialize, Serialize};

pub type Spanned<T>
where
    for<'a> T: Serialize + Deserialize<'a>,
= (T, SimpleSpan<usize, FileID>);

pub fn deserialize_static_str<'de, D>(deserializer: D) -> Result<&'static str, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let s: String = Deserialize::deserialize(deserializer)?;
    Ok(Box::leak(s.into_boxed_str()))
}

pub fn deserialize_static<'de, D, T>(deserializer: D) -> Result<&'static T, D::Error>
where
    D: serde::Deserializer<'de>,
    T: serde::Deserialize<'de>,
{
    let v: T = Deserialize::deserialize(deserializer)?;
    Ok(Box::leak(Box::new(v)))
}
