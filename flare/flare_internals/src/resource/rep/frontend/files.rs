use std::path::PathBuf;

/// Represents a file's unique identification code inside of a `Context`
pub type FileID = u64;

// #[derive(Debug, Clone)]
// pub struct FileSource<'src> {
//     pub filename: &'src Path,
//     pub src_text: &'src str,
// }

#[derive(Debug, Clone)]
pub struct FileSource {
    pub filepath: PathBuf,
    pub source: String,
    pub id: FileID,
}
