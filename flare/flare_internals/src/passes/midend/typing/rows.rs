use ena::unify::{EqUnifyValue, UnifyKey};

use crate::{passes::midend::typing::types::Type, resource::rep::ast::Label};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct RowVar(u32);

impl UnifyKey for RowVar {
    type Value = Option<Row>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        "RowVar"
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Row {
    Open(RowVar),
    Closed(ClosedRow),
}

impl EqUnifyValue for Row {}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ClosedRow {
    fields: &'static [Label],
    values: &'static [Type],
}

#[derive(Debug, Clone, Copy)]
pub struct RowCombination {
    pub left: Row,
    pub right: Row,
    pub goal: Row,
}
