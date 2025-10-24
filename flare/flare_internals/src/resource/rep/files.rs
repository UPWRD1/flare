use std::path::PathBuf;

/// Represents a file's unique identification code inside of a `Context`
pub type FileID = u64;

#[derive(Debug, Clone)]
pub struct FileSource {
    pub filename: PathBuf,
    pub src_text: String,
}
