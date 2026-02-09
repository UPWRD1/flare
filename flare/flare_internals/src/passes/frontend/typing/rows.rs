use std::{collections::BTreeSet, fmt::Display};

use ena::unify::{EqUnifyValue, UnifyKey};
use internment::Intern;
use itertools::Itertools;
use salsa::{interned, tracked};

use crate::{
    passes::frontend::typing::{Evidence, TyUniVar, TypeScheme, types::Type},
    resource::rep::{common::Spanned, frontend::ast::Label},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RowUniVar(u32);

impl UnifyKey for RowUniVar {
    type Value = Option<Row<'db>>;

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
// #[salsa::interned(debug)]
pub struct RowVar {
    name: Intern<String>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub enum Row {
    Open(RowVar),
    Unifier(RowUniVar),
    Closed(ClosedRow),
}

impl Spanned<Intern<Row<'_>>> {
    pub fn render(&self, scheme: &TypeScheme) -> String {
        self.0.render(scheme)
    }
}

impl Row<'_> {
    pub fn render(&self, scheme: &TypeScheme) -> String {
        match self {
            Self::Open(row_var) => format!("open{}", row_var.0),
            Self::Unifier(uni_var) => format!("rowunifier{}", uni_var.0),
            Self::Closed(closed_row) => closed_row
                .fields
                .iter()
                .zip(closed_row.values)
                .map(|(l, t)| format!("{}: {}", l.0.0, t.0.render(scheme)))
                .join(", "),
        }
    }
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

            // (Self::Unifier(_), Self::Closed(_)) | (Self::Closed(_), Self::Unifier(_)) => true,
            // Closed rows are equatable when they are subtypes?
            // (Self::Closed(a), Self::Closed(b)) => a.is_subtype_of(b).is_some(),
            // Anything else is not equatable
            _ => false,
        }
    }
}

impl Display for Row<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Open(row_var) => write!(f, "?{}", row_var.0),
            Self::Unifier(row_uni_var) => write!(f, "unifier{}", row_uni_var.0),
            Self::Closed(closed_row) => write!(f, "{closed_row}"),
        }
    }
}

impl EqUnifyValue for Row<'_> {}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClosedRow {
    pub fields: &'static [Label],
    pub values: &'static [Spanned<Intern<Type<'db>>>],
}

impl EqUnifyValue for ClosedRow<'_> {}

impl Display for ClosedRow<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.fields
                .iter()
                .zip(self.values.iter())
                .map(|(label, field)| format!("{}: {}", label.0.0, field))
                .join(", ")
        )
    }
}

impl ClosedRow<'_> {
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
                accum.push((me_label, me_v));
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
        for ty in self.values {
            if ty.0.mentions(unbound_tys, unbound_rows) {
                return true;
            }
        }
        false
    }

    pub fn sort(self) -> Self {
        let (fields, values): (Vec<Label>, Vec<Spanned<Intern<Type>>>) = self
            .fields
            .iter()
            .zip(self.values.iter())
            .sorted_by_key(|x| x.0.0.0)
            .unzip();
        let fields = fields.leak();
        let values = values.leak();
        Self { fields, values }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, salsa::Update)]
pub struct RowCombination<'db> {
    pub left: Spanned<&'db Row<'db>>,
    pub right: Spanned<&'db Row<'db>>,
    pub goal: Spanned<&'db Row<'db>>,
}

impl RowCombination<'_> {
    /// Two rows are unifiable if two of their components are equatable.
    /// A row can be uniquely determined by two of it's components (the third is calculated from
    /// the two). Because of this whenever rows agree on two components we can unify both rows and
    /// possible learn new information about the third row.
    ///
    /// This only works because our row combinations are commutative.
    pub fn is_unifiable(&self, other: &Self) -> bool {
        let left_equatable = self.left.0.equatable(&other.left.0);
        let right_equatable = self.right.0.equatable(&other.right.0);
        let goal_equatable = self.goal.0.equatable(&other.goal.0);
        (goal_equatable && (left_equatable || right_equatable))
            || (left_equatable && right_equatable)
    }

    /// Check unifiability the same way as `is_unifiable` but commutes the arguments.
    /// So we check left against right, and right against left. Goal is still checked against goal.
    pub fn is_comm_unifiable(&self, other: &Self) -> bool {
        let left_equatable = self.left.0.equatable(&other.right.0);
        let right_equatable = self.right.0.equatable(&other.left.0);
        let goal_equatable = self.goal.0.equatable(&other.goal.0);
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
