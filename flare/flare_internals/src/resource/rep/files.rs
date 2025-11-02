use std::path::Path;

/// Represents a file's unique identification code inside of a `Context`
pub type FileID = u64;

#[derive(Debug, Clone)]
pub struct FileSource<'src> {
    pub filename: &'src Path,
    pub src_text: &'src str,
}
