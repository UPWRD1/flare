use std::{fmt::Display, io::Cursor};

use ariadne::{sources, Color, Label, Report, ReportKind};

use chumsky::span::SimpleSpan;
use thiserror::Error;

pub type CompResult<T> = Result<T, CompilerErr>;
pub trait ReportableError {
    fn report(&self);
}


#[derive(Debug, Error)]
pub enum CompilerErr {
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

impl CompilerErr {
    pub fn get_dyn(self) -> DynamicErr {
        match self {
            CompilerErr::Dynamic(dynamic_err) => dynamic_err,
            _ => panic!("Cannot get dynamic err from {:?}", self),
        }
    }
}

impl ReportableError for CompilerErr {
    fn report(&self) {
        match self {
            CompilerErr::General(error) => error.report(),
            CompilerErr::Other(error) => eprintln!("{}", error),
            CompilerErr::Dynamic(e) => CompilerErr::General(e.clone().into()).report(),
            _ => todo!(),
        }
    }
}

impl From<std::io::Error> for CompilerErr {
    fn from(value: std::io::Error) -> Self {
        Self::Other(value.into())
    }
}

#[derive(Debug, Error, Clone)]
pub struct DynamicErr {
    msg: String,
    filename: Option<String>,
    label: Option<(String, SimpleSpan)>,
    extra_labels: Option<Vec<(String, SimpleSpan)>>,
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

    pub fn label(self, label: (String, SimpleSpan)) -> Self {
        Self {
            label: Some(label),
            ..self
        }
    }

    pub fn extra_labels(self, extra_labels: Vec<(String, SimpleSpan)>) -> Self {
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
}

impl std::fmt::Display for DynamicErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Into::<GeneralErr>::into(self.clone()))
    }
}

impl From<DynamicErr> for GeneralErr {
    fn from(value: DynamicErr) -> Self {
        GeneralErr {
            msg: value.msg,
            filename: value.filename.unwrap_or("".to_string()),
            label: value.label.unwrap_or(("".to_string(), SimpleSpan::from(0..0))),
            extra_labels: value.extra_labels.unwrap_or(vec![]),
            src: value.src.unwrap_or("".to_string()),
        }
        // value.msg,
        // value.label.unwrap_or(("error".to_string(), SimpleSpan::new(0, 0))),
        // None,
        // value.extra_labels,
        // value.src.unwrap_or_default(),
    }
}

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
struct GeneralErr {
    filename: String,
    msg: String,
    label: (String, SimpleSpan),
    extra_labels: Vec<(String, SimpleSpan)>,

    src: String,
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
        let fname = self.filename.clone();
        let mut buf = Cursor::new(vec![]);
        let mut rep = Report::build(
            ReportKind::Error,
            (fname.clone(), self.label.1.into_range()),
        )
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(&self.msg)
        .with_label(
            Label::new((fname.clone(), self.label.1.into_range()))
                .with_message(self.label.0.as_str())
                .with_color(Color::Red),
        )
        .with_labels(self.extra_labels.iter().map(|label2| {
            Label::new((fname.clone(), label2.1.into_range()))
                .with_message(label2.0.as_str())
                .with_color(Color::Yellow)
        }));

        rep.finish()
            .write(sources([(fname, self.src.clone())]), &mut buf)
            .unwrap();
        write!(
            f,
            "{}",
            String::from_utf8_lossy(buf.into_inner().as_slice())
        )
    }
}

impl ReportableError for GeneralErr {
    fn report(&self) {
        eprintln!("{}", self);
    }
}
