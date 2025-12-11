use std::{collections::BTreeSet, fmt::Display, hash};

use ena::unify::{EqUnifyValue, UnifyKey};
use internment::Intern;
use itertools::Itertools;

use crate::{
    passes::midend::typing::{Evidence, TyUniVar, types::Type},
    resource::rep::{Spanned, ast::Label},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RowUniVar(u32);

impl UnifyKey for RowUniVar {
    type Value = Option<Row>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        "RowUniVar"
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RowVar(pub u32);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Row {
    Open(RowVar),
    Unifier(RowUniVar),
    Closed(ClosedRow),
}

impl Row {
    pub fn single(lbl: Label, ty: Spanned<Intern<Type>>) -> Self {
        Self::Closed(ClosedRow {
            fields: vec![lbl].leak(),
            values: vec![ty].leak(),
        })
    }

    pub fn equatable(&self, other: &Self) -> bool {
        match (self, other) {
            // Unifier rows are equatable when their variables are equal
            (Self::Unifier(a), Self::Unifier(b)) => a == b,

            // Open rows are equatable when their variables are equal
            (Self::Open(a), Self::Open(b)) => a == b,

            // Closed rows are equatable when their fields are equal
            (Self::Closed(a), Self::Closed(b)) => a.fields == b.fields,

            // Closed rows are equatable when they are subtypes?
            // (Self::Closed(a), Self::Closed(b)) => a.is_subtype_of(b).is_some(),
            // Anything else is not equatable
            _ => false,
        }
    }
}

impl Display for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Open(row_var) => write!(f, "row_var{}", row_var.0),
            Self::Unifier(row_uni_var) => write!(f, "unifier{}", row_uni_var.0),
            Self::Closed(closed_row) => write!(f, "{closed_row}"),
        }
    }
}

impl EqUnifyValue for Row {}

#[derive(Copy, Clone, Debug)]
pub struct ClosedRow {
    pub fields: &'static [Label],
    pub values: &'static [Spanned<Intern<Type>>],
}

impl hash::Hash for ClosedRow {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.fields.hash(state);
        self.values.iter().for_each(|x| x.0.hash(state))
    }
}

impl PartialEq for ClosedRow {
    fn eq(&self, other: &Self) -> bool {
        self.is_subtype_of(other).is_some()
    }
}

impl Eq for ClosedRow {}

impl Ord for ClosedRow {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.fields
            .iter()
            .map(|x| x.0.0)
            .cmp(other.fields.iter().map(|x| x.0.0))
            .then(self.values.iter().cmp(other.values.iter()))
    }
}

impl PartialOrd for ClosedRow {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl EqUnifyValue for ClosedRow {}

impl Display for ClosedRow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.fields
                .iter()
                .zip(self.values.iter())
                .map(|(label, field)| format!("{}: {}", label.0.0, field))
                .join(",")
        )
    }
}

impl ClosedRow {
    pub fn is_subtype_of(&self, other: &Self) -> Option<Self> {
        let mut accum = vec![];
        for (me_label, me_v) in self.fields.iter().zip(self.values) {
            if let Some((idx, _)) = other
                .fields
                .iter()
                .enumerate()
                .find(|(_, other_label)| other_label.0.0 == me_label.0.0)
                && other.values[idx] == *me_v
            {
                accum.push((me_label, me_v))
            } else {
                return None;
            }
        }
        let (fields, values): (Vec<_>, Vec<_>) = accum.into_iter().unzip();
        let fields = fields.leak();
        let values = values.leak();
        Some(Self { fields, values })
    }

    pub fn merge(left: Self, right: Self) -> Self {
        // dbg!(left, right);
        let mut left_fields = left.fields.iter().peekable();
        let mut left_values = left.values.iter();
        let mut right_fields = right.fields.iter().peekable();
        let mut right_values = right.values.iter();

        let mut fields: Vec<Label> = vec![];
        let mut values: Vec<Spanned<Intern<Type>>> = vec![];

        // Since our input rows are already sorted we can explit that and not worry about resorting
        // them here, we just have to merge our two sorted rows.
        loop {
            match (left_fields.peek(), right_fields.peek()) {
                (Some(left), Some(right)) => {
                    if left.0.0.cmp(&right.0.0).is_lt() {
                        // SAFETY: We know the next item exists because we are matching against it.
                        unsafe { fields.push(*left_fields.next().unwrap_unchecked()) };

                        // SAFETY: Ditto ^^^
                        unsafe {
                            values.push(*left_values.next().unwrap_unchecked());
                        }
                    } else {
                        unsafe { fields.push(*right_fields.next().unwrap_unchecked()) };
                        unsafe { values.push(*right_values.next().unwrap_unchecked()) };
                    }
                }
                (Some(_), None) => {
                    fields.extend(left_fields);
                    values.extend(left_values);
                    break;
                }
                (None, Some(_)) => {
                    fields.extend(right_fields);
                    values.extend(right_values);
                    break;
                }
                (None, None) => {
                    break;
                }
            }
        }

        Self {
            fields: fields.leak(),
            values: values.leak(),
        }
    }
    pub fn mentions(
        &self,
        unbound_tys: &BTreeSet<TyUniVar>,
        unbound_rows: &BTreeSet<RowUniVar>,
    ) -> bool {
        for ty in self.values.iter() {
            if ty.0.mentions(unbound_tys, unbound_rows) {
                return true;
            }
        }
        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RowCombination {
    pub left: Row,
    pub right: Row,
    pub goal: Row,
}

impl RowCombination {
    /// Two rows are unifiable if two of their components are equatable.
    /// A row can be uniquely determined by two of it's components (the third is calculated from
    /// the two). Because of this whenever rows agree on two components we can unify both rows and
    /// possible learn new information about the third row.
    ///
    /// This only works because our row combinations are commutative.
    pub fn is_unifiable(&self, other: &Self) -> bool {
        let left_equatable = self.left.equatable(&other.left);
        let right_equatable = self.right.equatable(&other.right);
        let goal_equatable = self.goal.equatable(&other.goal);
        (goal_equatable && (left_equatable || right_equatable))
            || (left_equatable && right_equatable)
    }

    /// Check unifiability the same way as `is_unifiable` but commutes the arguments.
    /// So we check left against right, and right against left. Goal is still checked against goal.
    pub fn is_comm_unifiable(&self, other: &Self) -> bool {
        let left_equatable = self.left.equatable(&other.right);
        let right_equatable = self.right.equatable(&other.left);
        let goal_equatable = self.goal.equatable(&other.goal);
        (goal_equatable && (left_equatable || right_equatable))
            || (left_equatable && right_equatable)
    }

    pub fn into_evidence(self) -> Evidence {
        Evidence::RowEquation {
            left: self.left,
            right: self.right,
            goal: self.goal,
        }
    }
}
