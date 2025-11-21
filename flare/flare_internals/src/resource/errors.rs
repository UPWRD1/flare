use std::{
    any::Any,
    fmt::{self, Display},
    io::Cursor,
    ops::Deref,
    process::exit,
};

mod templates {

    use std::fmt::Display;

    use crate::resource::errors::DynamicErr;
    use crate::*;
    use chumsky::span::SimpleSpan;
    pub fn not_defined(q: impl Display, s: &SimpleSpan<usize, u64>) -> CompilerErr {
        let disp = q;
        DynamicErr::new(format!("Could not find a definition for '{}'", disp))
            .label(format!("'{}' not found in scope", disp), *s)
            //.src(self.src.to_string())
            .into()
    }

    pub fn bad_ident(item: impl Display) -> CompilerErr {
        DynamicErr::new(format!("Cannot get an identifier from a {}", item)).into()
    }
}

use rustc_hash::FxHashMap;
pub(crate) use templates::*;

use ariadne::{sources, Color, Label, Report, ReportKind};

use chumsky::span::{SimpleSpan, Span};
use thiserror::Error;

pub type CompResult<T> = Result<T, CompilerErr>;
pub trait ReportableError: Any + Display + std::error::Error + Send + Sync {
    fn report(&self, ctx: &FileCtx);
}

pub trait AnnotatableError: ReportableError {
    fn annotate<T>(&self, value: T) -> Self;
}

#[derive(Debug, Error)]
pub struct CompilerErr(Box<dyn ReportableError>);

use crate::{
    passes::midend::typing::{Row, TyUniVar, Type},
    resource::rep::files::FileSource,
    FileCtx, FileID,
};

impl Display for CompilerErr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl ReportableError for CompilerErr {
    fn report(&self, ctx: &FileCtx) {
        self.0.report(ctx)
    }
}

impl From<DynamicErr> for CompilerErr {
    fn from(value: DynamicErr) -> Self {
        Self(Box::new(CompilerErrKind::Dynamic(value)))
    }
}

impl From<ErrorCollection> for CompilerErr {
    fn from(value: ErrorCollection) -> Self {
        Self(Box::new(CompilerErrKind::ErrorCollection(value)))
    }
}

impl From<std::io::Error> for CompilerErr {
    fn from(value: std::io::Error) -> Self {
        Self(Box::new(CompilerErrKind::Other(value.into())))
    }
}

impl Deref for CompilerErr {
    type Target = dyn Any;
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

    #[error(transparent)]
    ErrorCollection(#[from] ErrorCollection),
    #[error(transparent)]
    Type(#[from] TypeErr),

    #[error(transparent)]
    Fatal(#[from] FatalErr),

    //#[error(transparent)]
    //Typecheck(#[from] TypecheckingError),
    // #[error(transparent)]
    // Environment(#[from] EnvironmentError),

    // Other error types...
    #[error(transparent)]
    Other(#[from] anyhow::Error), // Catch-all for unexpected errors
}

impl CompilerErrKind {
    pub fn get_dyn(self) -> DynamicErr {
        //panic!();

        match self {
            CompilerErrKind::Dynamic(dynamic_err) => dynamic_err,
            _ => panic!("Cannot get dynamic err from {:?}", self),
        }
    }
}

impl ReportableError for CompilerErrKind {
    fn report<'src>(&self, ctx: &FileCtx) {
        match self {
            CompilerErrKind::General(error) => eprintln!("{error}"),
            CompilerErrKind::Other(error) => eprintln!("{error}"),
            CompilerErrKind::Dynamic(e) => {
                CompilerErrKind::General(e.clone().get_gen(ctx)).report(ctx)
            }
            CompilerErrKind::ErrorCollection(errs) => {
                for e in &errs.0 {
                    e.report(ctx);
                }
            }
            CompilerErrKind::Type(error) => eprintln!("{error}"),
            CompilerErrKind::Fatal(_) => unreachable!(),
        }
    }
}

impl From<std::io::Error> for CompilerErrKind {
    fn from(value: std::io::Error) -> Self {
        Self::Other(value.into())
    }
}

#[derive(Debug, Error)]
pub struct ErrorCollection(Vec<CompilerErr>);

impl Display for ErrorCollection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for e in &self.0 {
            e.fmt(f)?;
        }
        Ok(())
    }
}

impl ErrorCollection {
    pub fn new(errs: Vec<CompilerErr>) -> Self {
        Self(errs)
    }
}

#[derive(Debug, Error, Clone)]
pub struct DynamicErr {
    //context: Option<Context>,
    msg: String,
    label: Option<(String, SimpleSpan<usize, FileID>)>,
    extra_labels: Option<Vec<(String, SimpleSpan<usize, FileID>)>>,
}

impl DynamicErr {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            msg: msg.into(),
            label: None,
            extra_labels: None,
        }
    }

    pub fn label(self, label: impl Into<String>, span: SimpleSpan<usize, FileID>) -> Self {
        Self {
            label: Some((label.into(), span)),
            ..self
        }
    }

    pub fn extra_labels(self, extra_labels: Vec<(String, SimpleSpan<usize, FileID>)>) -> Self {
        Self {
            extra_labels: Some(extra_labels),
            ..self
        }
    }

    pub fn generate_sources(&self, context: &FileCtx) -> Vec<(&'static str, &'static str)> {
        let mut source_ids: Vec<u64> = vec![];
        let label_origin = self.label.as_ref().unwrap().1.context;
        source_ids.push(label_origin);
        let mut extra_labels_origin: Vec<u64> = self
            .extra_labels
            .as_ref()
            .map_or_else(Vec::new, |v| v.iter().map(|x| x.1.context).collect());
        source_ids.append(&mut extra_labels_origin);
        let mut new_sources = vec![];
        for k in source_ids {
            let ent = context.get(&k).unwrap().clone();
            new_sources.push((ent.filename.to_str().unwrap(), ent.src_text))
        }

        new_sources
    }

    pub fn get_gen(self, context: &FileCtx) -> GeneralErr {
        let s = self.generate_sources(context);
        GeneralErr {
            msg: self.msg,
            label: self
                .label
                .unwrap_or(("here".to_string(), SimpleSpan::new(0, 0..0))),
            extra_labels: self.extra_labels.unwrap_or_default(),
            context: context.clone(),
            sources: s,
        }
    }
}

impl std::fmt::Display for DynamicErr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        write!(f, "Dynamic Error: {:?}", self)
    }
}
/// Opaque Error created from DynamicErr.
#[derive(Error, Debug)]
pub struct GeneralErr {
    msg: String,
    label: (String, SimpleSpan<usize, FileID>),
    extra_labels: Vec<(String, SimpleSpan<usize, FileID>)>,
    context: FxHashMap<FileID, FileSource<'static>>,
    sources: Vec<(&'static str, &'static str)>,
}

impl std::fmt::Display for GeneralErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = Cursor::new(vec![]);
        let rep = Report::build(
            ReportKind::Error,
            (self.sources.first().unwrap().0, self.label.1.into_range()),
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
                    self.context
                        .get(&self.label.1.context)
                        .unwrap()
                        .filename
                        .to_str()
                        .unwrap()
                },
                self.label.1.into_range(),
            ))
            .with_message(self.label.0.as_str())
            .with_color(Color::Red),
        )
        .with_labels(self.extra_labels.iter().map(|label2| {
            Label::new((
                self.context
                    .get(&label2.1.context)
                    .unwrap()
                    .filename
                    .to_str()
                    .unwrap(),
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

/// A wrapper struct for a fatal, terminating error.
/// Essentially a glorified wrapper for `panic!()`.
#[derive(Error, Debug)]
#[allow(dead_code)]
pub struct FatalErr(String);

impl ReportableError for FatalErr {
    fn report<'src>(&self, _: &FileCtx) {
        unreachable!()
    }
}

impl fmt::Display for FatalErr {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        unreachable!()
    }
}

impl FatalErr {
    /// Create a new `FatalErr`.
    /// Note that the act of creating a `FatalErr` terminates the program.
    #[inline]
    #[allow(clippy::new_ret_no_self)]
    pub fn new(msg: impl Display) -> ! {
        eprintln!("Uh oh!");
        eprintln!("flarec encountered a fatal error during compilation.");
        eprintln!("This is likely a bug within flarec.");
        eprintln!("Please file an issue here:");
        eprintln!("\thttps://github.com/UPWRD1/flare/issues/new/choose");

        eprintln!("Error details:");
        eprintln!("\t{msg}");
        eprintln!("flarec will now exit.");
        exit(1)
    }
}

impl From<FatalErr> for CompilerErr {
    fn from(_value: FatalErr) -> Self {
        unreachable!("FatalErr panics on creation, this should NEVER be called. This is here to satisfy rust's typechecker.")
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Error)]
pub enum TypeErr {
    InfiniteType {
        type_var: TyUniVar,
        ty: Type,
    },
    UnexpectedFun {
        expected_ty: Type,
        fun_ty: Type,
    },
    AppExpectedFun {
        inferred_ty: Type,
        expected_fun_ty: Type,
    },
    ExpectedUnify {
        checked: Type,
        inferred: Type,
    },
    TypeNotEqual(Type, Type),
    RowNotEqual(Row, Row),
}

impl Display for TypeErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl ReportableError for TypeErr {
    fn report(&self, ctx: &FileCtx) {
        eprintln!("{self}");
    }
}

impl From<TypeErr> for CompilerErr {
    fn from(value: TypeErr) -> Self {
        Self(Box::new(value))
    }
}
