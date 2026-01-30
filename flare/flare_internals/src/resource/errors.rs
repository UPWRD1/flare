use std::{
    any::Any,
    fmt::{self, Display},
    io::Cursor,
    ops::Deref,
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

    // pub fn bad_ident(item: impl Display) -> CompilerErr {
    //     DynamicErr::new(format!("Cannot get an identifier from a {}", item)).into()
    // }
}

use rustc_hash::FxHashMap;
pub(crate) use templates::*;

use ariadne::{Color, Label, Report, ReportKind, sources};

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
    FileCtx, FileID,
    passes::frontend::typing::{Row, TyUniVar, Type},
    resource::rep::frontend::files::FileSource,
};

impl Display for CompilerErr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl CompilerErr {
    pub fn report(&self, ctx: &FileCtx) {
        self.0.report(ctx)
    }
}

impl<T: ReportableError> From<T> for CompilerErr {
    fn from(value: T) -> Self {
        Self(Box::new(value))
    }
}

impl ReportableError for std::io::Error {
    fn report(&self, _ctx: &FileCtx) {
        eprintln!("{self}")
    }
}

impl Deref for CompilerErr {
    type Target = dyn Any;
    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
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

impl ReportableError for ErrorCollection {
    fn report(&self, ctx: &FileCtx) {
        for e in &self.0 {
            e.report(ctx);
        }
    }
}

impl From<Vec<CompilerErr>> for ErrorCollection {
    fn from(value: Vec<CompilerErr>) -> Self {
        Self(value)
    }
}

impl From<Vec<CompilerErr>> for CompilerErr {
    fn from(value: Vec<CompilerErr>) -> Self {
        Self(Box::new(ErrorCollection(value)))
    }
}

#[derive(Debug, Error, Clone)]
pub struct DynamicErr {
    //context: Option<Context>,
    msg: String,
    label: Option<(String, SimpleSpan<usize, FileID>)>,
    extra_labels: Vec<(String, SimpleSpan<usize, FileID>)>,
    help: Vec<String>,
}

impl DynamicErr {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            msg: msg.into(),
            label: None,
            extra_labels: vec![],
            help: vec![],
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
            extra_labels,
            ..self
        }
    }

    pub fn help(self, text: impl Into<String>) -> Self {
        Self {
            help: [self.help, vec![text.into()]].concat(),
            ..self
        }
    }

    pub fn extra(self, text: impl Into<String>, span: SimpleSpan<usize, FileID>) -> Self {
        Self {
            extra_labels: [self.extra_labels, vec![(text.into(), span)]].concat(),
            ..self
        }
    }
    pub fn generate_sources<'src>(&self, context: &FileCtx) -> Vec<(String, String)> {
        let mut source_ids: Vec<u64> = vec![];
        let label_origin = self
            .label
            .as_ref()
            .unwrap_or(&("here".to_string(), SimpleSpan::new(0, 0..0)))
            .1
            .context;
        source_ids.push(label_origin);
        let mut extra_labels_origin: Vec<u64> =
            self.extra_labels.iter().map(|x| x.1.context).collect();
        source_ids.append(&mut extra_labels_origin);
        let mut new_sources = vec![];
        for k in source_ids {
            let ent = context.get(&k).unwrap().clone();
            new_sources.push((ent.filepath.display().to_string(), ent.source))
        }

        new_sources
    }

    pub fn get_gen(self, context: &FileCtx) -> GeneralErr {
        // dbg!(&self);
        let s = self.generate_sources(context);
        GeneralErr {
            msg: self.msg,
            label: self
                .label
                .unwrap_or(("here".to_string(), SimpleSpan::new(0, 0..0))),
            extra_labels: self.extra_labels,
            help: self.help,
            context: context.clone(),
            sources: s,
        }
    }
}

impl ReportableError for DynamicErr {
    fn report(&self, ctx: &FileCtx) {
        let e = self.clone().get_gen(ctx);
        e.report(ctx)
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
    help: Vec<String>,
    context: FxHashMap<FileID, FileSource>,
    sources: Vec<(String, String)>,
}

impl ReportableError for GeneralErr {
    fn report(&self, _ctx: &FileCtx) {
        eprintln!("{self}")
    }
}

impl std::fmt::Display for GeneralErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = Cursor::new(vec![]);
        let mut rep = Report::build(
            ReportKind::Error,
            (
                self.sources.first().unwrap().0.clone(),
                self.label.1.into_range(),
            ),
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
                        .filepath
                        .display()
                        .to_string()
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
                    .filepath
                    .display()
                    .to_string(),
                label2.1.into_range(),
            ))
            .with_message(label2.0.as_str())
            .with_color(Color::Yellow)
        }));
        rep.with_helps(self.help.clone());
        let sources = sources(self.sources.clone());
        rep.finish().write(sources, &mut buf).unwrap();
        write!(
            f,
            "{}",
            String::from_utf8_lossy(buf.into_inner().as_slice())
        )
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
    fn report(&self, _ctx: &FileCtx) {
        eprintln!("{self}");
    }
}
