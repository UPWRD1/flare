use std::{io::Cursor, ops::Deref};

use ariadne::{sources, Color, Label, Report, ReportKind};

use chumsky::span::{SimpleSpan, Span};
use rayon::iter::ParallelIterator;
use thiserror::Error;

pub type CompResult<T> = Result<T, CompilerErr>;
pub trait ReportableError {
    fn report(&self, ctx: &Context);
}
#[derive(Debug, Error)]
pub struct CompilerErr(Box<CompilerErrKind>);

use std::fmt::Display;

use crate::{resource::rep::FileID, Context};

impl Display for CompilerErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl From<CompilerErrKind> for CompilerErr {
    fn from(value: CompilerErrKind) -> Self {
        Self(Box::new(value))
    }
}

impl From<DynamicErr> for CompilerErr {
    fn from(value: DynamicErr) -> Self {
        Self(Box::new(CompilerErrKind::Dynamic(value)))
    }
}

impl From<std::io::Error> for CompilerErr {
    fn from(value: std::io::Error) -> Self {
        Self(Box::new(CompilerErrKind::Other(value.into())))
    }
}

impl Deref for CompilerErr {
    type Target = CompilerErrKind;
    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

#[derive(Debug, Error)]
pub enum CompilerErrKind {
    #[error(transparent)]
    General(#[from] GeneralErr),

    #[error(transparent)]
    Dynamic(#[from] DynamicErr),

    // #[error(transparent)]
    // ErrorCollection(#[from] ErrorCollection),

    //#[error(transparent)]
    //Typecheck(#[from] TypecheckingError),
    // #[error(transparent)]
    // Environment(#[from] EnvironmentError),

    // Other error types...
    #[error(transparent)]
    Other(#[from] anyhow::Error), // Catch-all for unexpected errors
}

impl CompilerErrKind {
    pub fn get_dyn(&self) -> DynamicErr {
        //panic!();

        match self {
            CompilerErrKind::Dynamic(dynamic_err) => dynamic_err.clone(),
            _ => panic!("Cannot get dynamic err from {:?}", self),
        }
    }
}

impl ReportableError for CompilerErrKind {
    fn report(&self, ctx: &Context) {
        match self {
            CompilerErrKind::General(error) => eprintln!("{}", error),
            CompilerErrKind::Other(error) => eprintln!("{}", error),
            CompilerErrKind::Dynamic(e) => {
                CompilerErrKind::General(e.clone().get_gen(ctx)).report(&ctx)
            } //_ => todo!(),
        }
    }
}

impl From<std::io::Error> for CompilerErrKind {
    fn from(value: std::io::Error) -> Self {
        Self::Other(value.into())
    }
}

#[derive(Debug, Error, Clone)]
pub struct DynamicErr {
    //context: Option<Context>,
    msg: String,
    filename: Option<String>,
    label: Option<(String, SimpleSpan<usize, FileID>)>,
    extra_labels: Option<Vec<(String, SimpleSpan<usize, FileID>)>>,
    src: Option<String>,
}

impl DynamicErr {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            msg: msg.into(),
            filename: None,
            label: None,
            extra_labels: None,
            src: None,
        }
    }

    pub fn label(self, label: (String, SimpleSpan<usize, FileID>)) -> Self {
        Self {
            label: Some(label),
            ..self
        }
    }

    pub fn extra_labels(self, extra_labels: Vec<(String, SimpleSpan<usize, FileID>)>) -> Self {
        Self {
            extra_labels: Some(extra_labels),
            ..self
        }
    }

    pub fn src(self, src: impl Into<String>) -> Self {
        Self {
            src: Some(src.into()),
            ..self
        }
    }

    pub fn filename(self, filename: impl Into<String>) -> Self {
        Self {
            filename: Some(filename.into()),
            ..self
        }
    }

    pub fn generate_sources(&self, context: &Context) -> Vec<(String, String)> {
        let mut source_ids: Vec<u64> = vec![];
        let label_origin = self.label.as_ref().unwrap().1.context;
        source_ids.push(label_origin);
        let mut extra_labels_origin: Vec<u64> = self
            .extra_labels
            .as_ref()
            .map_or_else(||Vec::new(), |v| v.iter()
            .map(|x| x.1.context)
            .collect())
            ;
        source_ids.append(&mut extra_labels_origin);
        let mut new_sources = vec![];
        for k in source_ids {
            let contex = context.filectx.lock().unwrap();
            let ent = contex.get(&k).unwrap().clone();
            new_sources.push((
                ent.filename.to_str().unwrap().to_string(),
                ent.src_text.clone(),
            ))
        }

        new_sources
    }

    pub fn get_gen(self, context: &Context) -> GeneralErr {
        let s = self.generate_sources(context);
        GeneralErr {
            msg: self.msg,
            filename: self.filename.unwrap_or("unknown".to_string()),
            label: self
                .label
                .unwrap_or(("here".to_string(), SimpleSpan::new(0, 0..0))),
            extra_labels: self.extra_labels.unwrap_or_default(),
            src: self.src.unwrap_or("".to_string()),
            context: context.clone(),
            sources: s,
        }
    }
}

impl std::fmt::Display for DynamicErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Dynamic Error: {:?}", self)
    }
}

// impl From<DynamicErr> for GeneralErr {
//     fn from(value: DynamicErr) -> Self {
//         let me = value.clone().generate_sources();
//         GeneralErr {
//             msg: value.msg,
//             filename: value.filename.unwrap_or("unknown".to_string()),
//             label: value.label.unwrap_or(("here".to_string(), SimpleSpan::new(0, 0..0))),
//             extra_labels: value.extra_labels.unwrap_or_default(),
//             src: value.src.unwrap_or("".to_string()),
//             sources: me.unwrap_or(),
//         }

//         // value.msg,
//         // value.label.unwrap_or(("error".to_string(), SimpleSpan::new(0, 0))),
//         // None,
//         // value.extra_labels,
//         // value.src.unwrap_or_default(),
//     }
// }

// #[derive(Error, Debug)]
// pub struct ErrorCollection {
//     pub errors: Vec<CompilerErr>,
// }

// impl Display for ErrorCollection {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         for e in &self.errors {
//             e.fmt(f)?
//         }
//         Ok(())
//     }
// }

// impl From<Vec<CompilerErr>> for ErrorCollection {
//     fn from(value: Vec<CompilerErr>) -> Self {
//         Self { errors: value }
//     }
// }

// impl ReportableError for ErrorCollection {
//     fn report(&self) {
//         for e in &self.errors {
//             e.report();
//         }
//     }
// }

/// Opaque Error created from DynamicErr.
#[derive(Error, Debug, Clone)]
pub struct GeneralErr {
    filename: String,
    msg: String,
    label: (String, SimpleSpan<usize, FileID>),
    extra_labels: Vec<(String, SimpleSpan<usize, FileID>)>,

    src: String,
    context: Context,
    sources: Vec<(String, String)>,
}

// impl GeneralErr {
//     pub fn new(
//         msg: impl Into<String>,
//         label: (String, SimpleSpan),
//         filename: Option<impl Into<String>>,
//         extra_labels: Option<Vec<(String, SimpleSpan)>>,
//         src: String,
//     ) -> Self {
//         Self {
//             filename: filename.and_then(|f| Some(f.into())),
//             msg: msg.into(),
//             label,
//             extra_labels,
//             src: Some(src),
//         }
//     }
// }

impl std::fmt::Display for GeneralErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = Cursor::new(vec![]);
        let rep = Report::build(
            ReportKind::Error,
            (self.filename.clone(), self.label.1.into_range()),
        )
        .with_config(
            ariadne::Config::new()
                .with_index_type(ariadne::IndexType::Byte)
                .with_label_attach(ariadne::LabelAttach::Middle),
        )
        .with_message(&self.msg)
        .with_label(
            Label::new((
                {
                    let handle = self.context.filectx.lock().unwrap();
                    let res = handle
                        .get(&self.label.1.context)
                        .unwrap()
                        .filename
                        .to_str()
                        .unwrap()
                        .to_string();
                    drop(handle);
                    res
                },
                self.label.1.into_range(),
            ))
            .with_message(self.label.0.as_str())
            .with_color(Color::Red),
        )
        .with_labels(self.extra_labels.iter().map(|label2| {
            Label::new((
                self.context
                    .filectx
                    .lock()
                    .unwrap()
                    .get(&label2.1.context)
                    .unwrap()
                    .filename
                    .to_str()
                    .unwrap()
                    .to_string(),
                label2.1.into_range(),
            ))
            .with_message(label2.0.as_str())
            .with_color(Color::Yellow)
        }));

        rep.finish()
            .write(sources(self.sources.clone()), &mut buf)
            .unwrap();
        write!(
            f,
            "{}",
            String::from_utf8_lossy(buf.into_inner().as_slice())
        )
    }
}

// impl ReportableError for GeneralErr {
//     fn report(&self, ctx: Context) {
//         eprintln!("{}", self);
//     }
// }
