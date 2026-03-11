use internment::Intern;
use itertools::Itertools;
use ordered_float::OrderedFloat;

use crate::resource::rep::{
    common::Variable,
    frontend::{
        ast::{Label, Untyped},
        cst::{CstExpr, Pattern},
    },
};

/// Pattern matching compiler — Rust port of the OCaml original.
//
// Compiles a list of typed patterns into a decision tree via
// a pattern matrix algorithm (similar to Maranget 2008).
use std::collections::{HashMap, HashSet};

// ---------------------------------------------------------------------------
// Occurrences  (paths into the scrutinee)
// ---------------------------------------------------------------------------

/// A node in the occurrence tree that describes *where* a sub-value lives.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Occ {
    /// Root variable.
    Base(String),
    /// i-th projection of an occurrence.
    Proj(Box<Self>, usize),
    /// Unwrap the payload of a constructor at an occurrence.
    Unwrap(Box<Self>),
}

impl Occ {
    pub fn base(name: impl Into<String>) -> Self {
        Self::Base(name.into())
    }

    pub fn default_occ() -> Self {
        Self::Base("unknown".to_string())
    }
}

// ---------------------------------------------------------------------------
// Patterns
// ---------------------------------------------------------------------------

impl<V: Variable> Pattern<V> {
    pub fn wildcard() -> Self {
        Self::Any
    }

    pub fn is_refutable(&self) -> bool {
        matches!(
            self,
            Self::Variant(_, _)
                | Self::Number(_)
                | Self::String(_)
                | Self::Unit
                | Self::Bool(_)
                | Self::Particle(_)
        )
    }

    pub fn is_irrefutable(&self) -> bool {
        !self.is_refutable()
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self, Self::Any | Self::Var(_))
    }

    pub fn show(&self) -> String {
        match &self {
            Self::Any => "_".to_string(),
            Self::Var(x) => x.to_string(),
            Self::Variant(c, p) => format!("{} {}", c.0.0, p.0.show()),
            Self::Tuple(ps) => {
                let parts: Vec<_> = ps.iter().map(|p| p.0.show()).collect();
                format!("({})", parts.join(","))
            }
            Self::Number(n) => n.to_string(),
            Self::String(s) => format!("\"{}\"", s.0),
            _ => todo!(),
        }
    }
}

// ---------------------------------------------------------------------------
// Pattern matrix
// ---------------------------------------------------------------------------

/// A pattern matrix: a rectangular grid of typed patterns plus, per row,
/// the index of the arm that should fire if the row matches.
#[derive(Debug, Clone)]
pub struct Matrix {
    /// Column headers — one occurrence per column.
    pub header: Vec<Occ>,
    /// Each row is `(patterns, arm_index)`.
    pub rows: Vec<(Vec<Pattern<Untyped>>, usize)>,
}

impl Matrix {
    pub fn new(header: Vec<Occ>) -> Self {
        Self {
            header,
            rows: vec![],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    pub fn num_cols(&self) -> usize {
        self.header.len()
    }

    /// Borrow the i-th row's pattern slice.
    pub fn row_pats(&self, i: usize) -> &[Pattern<Untyped>] {
        &self.rows[i].0
    }

    /// Swap columns `a` and `b` (header + every row).
    pub fn swap_columns(&mut self, a: usize, b: usize) {
        if a == b {
            return;
        }
        self.header.swap(a, b);
        for (row, _) in &mut self.rows {
            row.swap(a, b);
        }
    }

    /// Return the index of the first column whose patterns satisfy `pred`.
    pub fn find_first_column(&self, pred: impl Fn(&[Pattern<Untyped>]) -> bool) -> usize {
        for i in 0..self.num_cols() {
            let col: Vec<&_> = self.rows.iter().map(|(r, _)| &r[i]).collect();
            // Re-use pred with owned slice — collect to owned for simplicity.
            let owned: Vec<_> = col.into_iter().copied().collect();
            if pred(&owned) {
                return i;
            }
        }
        0
    }

    /// All patterns in column `col`.
    pub fn column(&self, col: usize) -> Vec<Pattern<Untyped>> {
        self.rows.iter().map(|(r, _)| r[col]).collect()
    }

    pub fn print(&self) -> String {
        let mut accum: String = String::new();
        for s in &self.header {
            accum = format!("{}{:?},\t", accum, s)
        }
        let len = accum.len();
        let divider = format!("{:-^1$}", "", len);
        accum = format!("{accum}\n{divider}\n");
        for (pat, _) in &self.rows {
            accum = format!("{accum}|{}\n", pat.iter().map(|p| p.show()).join("\t|\t"))
        }
        accum
    }
}

// ---------------------------------------------------------------------------
// Decision tree
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum DecisionTree {
    /// No arm matched — this branch is unreachable / non-exhaustive.
    Fail,
    /// Fire arm `i`.
    Leaf(usize),
    /// Test `occ`; branch on constructor tag; fall back to `default` if no
    /// case matches (i.e. signature is incomplete).
    Switch {
        occ: Occ,
        cases: Vec<(SigElem, Self)>,
        default: Option<Box<Self>>,
    },
}

impl DecisionTree {
    /// Pretty-print the tree (mirrors the OCaml `Graphviz.print` hook).
    pub fn print(&self, indent: usize) {
        let pad = " ".repeat(indent * 2);
        match self {
            Self::Fail => println!("{pad}Fail"),
            Self::Leaf(i) => println!("{pad}Leaf({i})"),
            Self::Switch {
                occ,
                cases,
                default,
            } => {
                println!("{pad}Switch({occ:?})");
                for (ctor, sub) in cases {
                    println!("{pad}  | {} =>", ctor.print());
                    sub.print(indent + 2);
                }
                if let Some(def) = default {
                    println!("{pad}  | _ =>");
                    def.print(indent + 2);
                }
            } // println!("{pad}test")
        }
    }
}

fn occurrences_of(p: &Pattern<Untyped>, base: &Occ) -> Vec<(Occ, Pattern<Untyped>)> {
    match &p {
        Pattern::Tuple(sub_ps) => sub_ps
            .iter()
            .enumerate()
            .map(|(i, sub_p)| {
                let occ = Occ::Proj(Box::new(base.clone()), i);
                (occ, *sub_p.0)
            })
            .collect(),
        _ => vec![(base.clone(), *p)],
    }
} // ---------------------------------------------------------------------------
// preprocess: build the initial matrix from a flat list of patterns
// ---------------------------------------------------------------------------

/// Expand each top-level pattern into a map `Occ → Pattern<_>`, unfolding tuples
/// into their projections, then assemble a `Matrix`.
///
/// `res` maps a row index (0-based) to its arm index.
pub fn preprocess(base: &Occ, ps: &[Pattern<Untyped>], res: impl Fn(usize) -> usize) -> Matrix {
    // Build the header (ordered, deduplicated list of occurrences).
    let mut seen: Vec<Occ> = Vec::new();
    let mut header: Vec<Occ> = Vec::new();
    let all_pairs: Vec<Vec<(Occ, Pattern<_>)>> = ps
        .iter()
        .map(|p| {
            let pairs = occurrences_of(p, base);
            for (occ, _) in &pairs {
                if !seen.contains(occ) {
                    seen.push(occ.clone());
                    header.push(occ.clone());
                }
            }
            pairs
        })
        .collect();

    // Populate rows.
    let mut matrix = Matrix::new(header.clone());
    for (i, pairs) in all_pairs.into_iter().enumerate() {
        let map: HashMap<_, _> = pairs.into_iter().collect();
        let row: Vec<Pattern<_>> = header
            .iter()
            .map(|occ| map.get(occ).copied().unwrap_or_else(Pattern::wildcard))
            .collect();
        matrix.rows.push((row, res(i)));
    }
    matrix
}

// ---------------------------------------------------------------------------
// specialise: filter + transform rows for a specific head constructor
// ---------------------------------------------------------------------------

/// Strip the first column from every row whose head pattern satisfies
/// `pred`, unwrap the payload (if any), and re-preprocess the result.
/// The remaining columns are reattached afterwards.
fn specialise(matrix: &Matrix, pred: impl Fn(&Pattern<Untyped>) -> bool) -> Matrix {
    let mut patterns: Vec<Pattern<_>> = Vec::new();
    let mut indices: Vec<usize> = Vec::new();
    let mut remainders: Vec<Vec<Pattern<_>>> = Vec::new();

    for (row, idx) in &matrix.rows {
        let head = &row[0];
        if pred(head) {
            let unwrapped = unwrap_payload(*head);
            patterns.push(unwrapped);
            indices.push(*idx);
            remainders.push(row[1..].to_vec());
        }
    }

    let occ = Occ::Unwrap(Box::new(matrix.header[0].clone()));

    // Re-run preprocessing on the unwrapped payloads.
    let idx_snapshot = indices.clone();
    let mut m2 = preprocess(&occ, &patterns, move |i| idx_snapshot[i]);

    // Append the tail columns from the original header.
    for h in &matrix.header[1..] {
        m2.header.push(h.clone());
    }

    // Append the remainder columns to each row.
    for (i, (row, _)) in m2.rows.iter_mut().enumerate() {
        if let Some(rem) = remainders.get(i) {
            row.extend_from_slice(rem);
        }
    }

    m2
}

/// Unwrap the payload of a constructor pattern.
/// Sets `ty_out` to the payload's type on the first call.
fn unwrap_payload(p: Pattern<Untyped>) -> Pattern<Untyped> {
    match p {
        Pattern::Variant(_, inner) => *inner.0,
        Pattern::Number(_) | Pattern::String(_) | Pattern::Bool(_) | Pattern::Particle(_) => {
            Pattern::Any
        }
        _ => p,
    }
}

// ---------------------------------------------------------------------------
// Helper predicates
// ---------------------------------------------------------------------------

/// Does pattern `p` admit constructor `c`?  
/// (Wildcards / variables admit everything.)
fn admits(c: &SigElem, p: &Pattern<Untyped>) -> bool {
    match c {
        SigElem::Label(c) => match &p {
            Pattern::Variant(c2, _) => c == c2,
            Pattern::Any | Pattern::Var(_) => true,
            _ => false,
        },
        SigElem::Num(n) => match &p {
            Pattern::Number(n2) => n == n2,
            Pattern::Any | Pattern::Var(_) => true,
            _ => false,
        },
        SigElem::String(_) => todo!(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SigElem {
    Label(Label),
    Num(OrderedFloat<f64>),
    String(Intern<String>),
}

impl SigElem {
    fn print(&self) -> String {
        match self {
            Self::Label(label) => label.0.0.to_string(),
            Self::Num(ordered_float) => ordered_float.0.to_string(),
            Self::String(intern) => todo!(),
        }
    }
}

/// Collect every constructor name mentioned in a column.
fn collect_signature(ps: &[Pattern<Untyped>]) -> HashSet<SigElem> {
    ps.iter()
        .filter_map(|p| match &p {
            Pattern::Variant(c, p) => Some(SigElem::Label(*c)),
            Pattern::Number(f) => Some(SigElem::Num(*f)),
            Pattern::String(s) => Some(SigElem::String(s.0)),

            _ => None,
        })
        .collect()
}

/// True iff any pattern in the slice is refutable.
fn has_refutable(ps: &[Pattern<Untyped>]) -> bool {
    ps.iter().any(|p| p.is_refutable())
}

// ---------------------------------------------------------------------------
// compile: entry point
// ---------------------------------------------------------------------------

/// Compile `ps` (one per arm, 0-indexed) into a `DecisionTree`.
///
/// `arities` maps each type-constructor name to the number of data
/// constructors it has (used to detect exhaustiveness).
pub fn compile(base: &Occ, ps: &[Pattern<Untyped>]) -> DecisionTree {
    let initial = preprocess(base, ps, |i| i);
    compile_matrix(initial)
}

fn compile_matrix(matrix: Matrix) -> DecisionTree {
    println!("{}", matrix.print());
    // Base case 1: no rows → no match.
    if matrix.is_empty() {
        return DecisionTree::Fail;
    }

    // Base case 2: first row is all wildcards → it fires unconditionally.
    if matrix.row_pats(0).iter().all(Pattern::is_irrefutable) {
        return DecisionTree::Leaf(matrix.rows[0].1);
    }

    // Recursive case: choose the first column with a refutable pattern,
    // swap it to position 0, then case-split on its constructor signature.
    let mut matrix = matrix;
    let col_idx = matrix.find_first_column(has_refutable);
    matrix.swap_columns(0, col_idx);

    let first_col = matrix.column(0);
    let signature = collect_signature(&first_col);
    let occ = matrix.header[0].clone();

    // Build one branch per constructor in the signature.
    let cases: Vec<(SigElem, DecisionTree)> = {
        let mut v: Vec<_> = signature
            .iter()
            .map(|c| {
                let sub = specialise(&matrix, move |p| admits(c, p));
                (*c, compile_matrix(sub))
            })
            .collect();
        // Sort for deterministic output.
        v.sort_by(|(a, _), (b, _)| a.cmp(b));
        v
    };

    // Add a default arm when the signature does not cover the type.
    let default = {
        let sub = specialise(&matrix, Pattern::is_wildcard);
        Some(Box::new(compile_matrix(sub)))
        // None
    };

    DecisionTree::Switch {
        occ,
        cases,
        default,
    }
}

struct PatMatrix<const Occs: usize, const Acts: usize>([[Pattern<Untyped>; Acts]; Occs]);

struct MatchCompiler<const Occs: usize, const Acts: usize, V: Variable> {
    occs: [CstExpr<V>; Occs],
    actions: [CstExpr<V>; Acts],
    matrix: PatMatrix<Occs, Acts>,
}
// ---------------------------------------------------------------------------
// Demo / smoke test
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // fn cons(name: &str) -> Pattern<Untyped> {
    //     Pattern::Variant(name.to_string(), None)
    // }

    // fn cons_payload(name: &str, payload: Pattern<Untyped>) -> Pattern<_> {
    //     Pattern::Variant(name.to_string(), Some(Box::new(payload)))
    // }

    // fn wild() -> Pattern<_> {
    //     Pattern::wildcard()
    // }

    // #[test]
    // fn test_option_exhaustive() {
    //     // match x with | Some _ -> 0 | None -> 1

    //     let ps = vec![cons_payload("Some", wild()), cons("None")];

    //     let base = Occ::Base("x".to_string());
    //     let tree = compile(base, &ps);
    //     tree.print(0);

    //     matches!(tree, DecisionTree::Switch { .. });
    // }

    // #[test]
    // fn test_wildcard_only() {
    //     // match x with | _ -> 0
    //     let ps = vec![wild()];
    //     let base = Occ::Base("x".to_string());
    //     let tree = compile(base, &ps);
    //     assert!(matches!(tree, DecisionTree::Leaf(0)));
    // }

    // #[test]
    // fn test_non_exhaustive() {
    //     // match x with | True -> 0     (missing False)
    //     let ps = vec![cons("True")];
    //     let base = Occ::Base("x".to_string());
    //     let tree = compile(base, &ps);
    //     // Should have a default Fail branch for the missing constructor.
    //     if let DecisionTree::Switch { default, .. } = &tree {
    //         assert!(matches!(default.as_deref(), Some(DecisionTree::Fail)));
    //     } else {
    //         panic!("expected Switch node");
    //     }
    // }
}
