use std::{
    any::Any,
    fmt::{self, Display},
    io::Cursor,
};

mod templates {

    use std::fmt::Display;

    use crate::resource::errors::DynamicErr;
    use crate::*;
    use chumsky::span::SimpleSpan;
    pub fn not_defined(q: impl Display, s: &SimpleSpan<usize, u64>) -> DynamicErr {
        let disp = q;
        DynamicErr::new(format!("Could not find a definition for '{}'", disp))
            .label(format!("'{}' not found in scope", disp), *s)
        //.src(self.src.to_string())
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

pub type CompResult<T> = Result<T, DynamicErr>;
// pub trait AnnotatableError: ReportableError {
// fn annotate<T>(&self, value: T) -> Self;
// }

// #[derive(Debug, Error)]
// pub struct CompilerErr(Box<dyn ReportableError>);

use crate::{
    FileCtx, FileID,
    passes::frontend::typing::{Row, TyUniVar, Type},
    resource::rep::frontend::files::FileSource,
};

// impl Display for CompilerErr {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         self.0.fmt(f)
//     }
// }

// impl CompilerErr {
//     pub fn report(&self, ctx: &FileCtx) {
//         self.0.report(ctx)
//     }
// }

// impl<T: ReportableError> From<T> for CompilerErr {
//     fn from(value: T) -> Self {
//         Self(Box::new(value))
//     }
// }

// impl ReportableError for std::io::Error {
//     fn report(&self, _ctx: &FileCtx) {
//         eprintln!("{self}")
//     }
// }

// impl Deref for CompilerErr {
//     type Target = dyn Any;
//     fn deref(&self) -> &Self::Target {
//         self.0.as_ref()
//     }
// }

#[salsa::accumulator]
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
    pub fn render(self, db: &dyn salsa::Database, ctx: FileCtx<'_>) -> String {
        GeneralErr {
            msg: self.msg,
            label: self.label.unwrap(),
            extra_labels: self.extra_labels,
            help: self.help,
        }
        .render(db, ctx)
    }
}

impl std::fmt::Display for DynamicErr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        write!(f, "Dynamic Error: {:?}", self)
    }
}
/// Opaque Error created from DynamicErr.
#[derive(Error, Debug, salsa::Update)]
struct GeneralErr {
    msg: String,
    label: (String, SimpleSpan<usize, FileID>),
    extra_labels: Vec<(String, SimpleSpan<usize, FileID>)>,
    help: Vec<String>,
}

impl<'db> GeneralErr {
    fn generate_sources(
        &self,
        db: &'db dyn salsa::Database,
        ctx: FileCtx,
    ) -> Vec<(String, String)> {
        let mut source_ids: Vec<u64> = vec![];
        let label_origin = self.label.1.context;
        source_ids.push(label_origin);
        let mut extra_labels_origin: Vec<u64> =
            self.extra_labels.iter().map(|x| x.1.context).collect();
        source_ids.append(&mut extra_labels_origin);
        let mut new_sources = vec![];
        for k in source_ids {
            let ent = ctx.cache(db).get(&k).unwrap().clone();
            new_sources.push((
                ent.filepath(db).display().to_string(),
                ent.source(db).clone(),
            ))
        }

        new_sources
    }
    fn render(self, db: &'db dyn salsa::Database, ctx: FileCtx) -> String {
        let source_txts = self.generate_sources(db, ctx);
        let mut buf = Cursor::new(vec![]);
        let mut rep = Report::build(
            ReportKind::Error,
            (
                source_txts.first().unwrap().0.clone(),
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
                    ctx.cache(db)
                        .get(&self.label.1.context)
                        .unwrap()
                        .filepath(db)
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
                ctx.cache(db)
                    .get(&label2.1.context)
                    .unwrap()
                    .filepath(db)
                    .display()
                    .to_string(),
                label2.1.into_range(),
            ))
            .with_message(label2.0.as_str())
            .with_color(Color::Yellow)
        }));
        rep.with_helps(self.help.clone());
        let sources = sources(source_txts.clone());
        rep.finish().write(sources, &mut buf).unwrap();
        String::from_utf8_lossy(buf.into_inner().as_slice()).to_string()
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
