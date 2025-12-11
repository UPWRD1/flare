mod check;
mod infer;
mod inst;
mod rows;
mod subst;
mod types;
mod unify;

use chumsky::span::SimpleSpan;
pub use rows::{ClosedRow, Row, RowVar};
use rows::{RowCombination, RowUniVar};
pub use types::{TyUniVar, Type, TypeVar};

use std::{collections::BTreeSet, fmt::Display, hash::Hash};

use ena::unify::InPlaceUnificationTable;
use internment::Intern;

use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};

use crate::{
    passes::midend::typing::subst::SubstOut,
    resource::{
        errors::{CompResult, CompilerErr, DynamicErr},
        rep::{
            Spanned,
            ast::{Expr, ItemId, NodeId, Untyped, Variable},
            common::Ident,
        },
    },
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Typed(pub Untyped, pub Spanned<Intern<Type>>);

impl Variable for Typed {}

impl Display for Typed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1)
    }
}

impl Ident for Typed {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        self.0.ident()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Constraint {
    TypeEqual(Provenance, Spanned<Intern<Type>>, Spanned<Intern<Type>>),
    RowCombine(Provenance, RowCombination),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Evidence {
    RowEquation { left: Row, right: Row, goal: Row },
}

#[derive(Debug, Clone, Copy)]
pub enum Provenance {
    // A non function type encountered a Fun ast node, causing a type mismatch.
    UnexpectedFun(NodeId),
    // An application has an ast node in function position that does not have a function type.
    AppExpectedFun(NodeId),
    // Constraint produced by subsumption.
    ExpectedUnify(NodeId, NodeId),
    //
    ExpectedCombine(NodeId),
}

impl Provenance {
    fn id(&self) -> NodeId {
        match self {
            Self::UnexpectedFun(node_id)
            | Self::AppExpectedFun(node_id)
            | Self::ExpectedCombine(node_id)
            | Self::ExpectedUnify(node_id, _) => *node_id,
        }
    }
}

#[derive(Debug)]
pub struct GenOut {
    constraints: Vec<Constraint>,
    typed_ast: Spanned<Intern<Expr<Typed>>>,
    // inference_base_loc: Option<SimpleSpan<usize, u64>>,
}

impl GenOut {
    fn new(constraints: Vec<Constraint>, typed_ast: Spanned<Intern<Expr<Typed>>>) -> Self {
        Self {
            constraints,
            typed_ast,
            // inference_base_loc: None,
        }
    }

    fn with_typed_ast(
        self,
        f: impl FnOnce(Spanned<Intern<Expr<Typed>>>) -> Spanned<Intern<Expr<Typed>>>,
    ) -> Self {
        Self {
            constraints: self.constraints,
            typed_ast: f(self.typed_ast),
            // inference_base_loc: self.inference_base_loc,
        }
    }

    // fn with_infer_loc(self, loc: SimpleSpan<usize, u64>) -> Self {
    //     Self {
    //         constraints: self.constraints,
    //         typed_ast: self.typed_ast,
    //         inference_base_loc: Some(loc),
    //     }
    // }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct TypeScheme {
    pub unbound_types: BTreeSet<TypeVar>,

    pub unbound_rows: BTreeSet<RowVar>,
    pub evidence: Vec<Evidence>,
    pub ty: Spanned<Intern<Type>>,
}

#[derive(Debug)]
pub struct TypeInferOut {
    pub ast: Spanned<Intern<Expr<Typed>>>,
    pub scheme: TypeScheme,
    pub errors: FxHashMap<NodeId, CompilerErr>,
    pub row_to_ev: FxHashMap<NodeId, Evidence>,
    pub branch_to_ret_ty: FxHashMap<NodeId, Spanned<Intern<Type>>>,
    pub item_wrappers: FxHashMap<NodeId, ItemWrapper>,
}

#[derive(Debug)]
pub struct TypesOutput {
    pub typed_ast: Spanned<Intern<Expr<Typed>>>,
    pub scheme: TypeScheme,
    pub errors: FxHashMap<NodeId, CompilerErr>,
    pub row_to_ev: FxHashMap<NodeId, Evidence>,
    pub branch_to_ret_ty: FxHashMap<NodeId, Spanned<Intern<Type>>>,
    pub item_wrappers: FxHashMap<NodeId, ItemWrapper>,
}

#[derive(Debug, Clone)]
pub struct ItemWrapper {
    pub types: Vec<Spanned<Intern<Type>>>,
    pub rows: Vec<Row>,
    pub evidence: Vec<Evidence>,
}

#[derive(Default, Debug)]
pub struct ItemSource {
    pub types: FxHashMap<ItemId, TypeScheme>,
}

impl ItemSource {
    pub fn new(types: FxHashMap<ItemId, TypeScheme>) -> Self {
        Self { types }
    }

    fn type_of_item(&self, item_id: ItemId) -> TypeScheme {
        // dbg!(item_id);
        // dbg!(self);
        self.types[&item_id].clone()
    }

    pub fn insert(&mut self, k: ItemId, v: TypeScheme) -> Option<TypeScheme> {
        self.types.insert(k, v)
    }
}

pub struct Solver<'env> {
    tables: SolverTables,
    item_source: &'env ItemSource,
}

#[derive(Default)]
struct SolverTables {
    unification_table: InPlaceUnificationTable<TyUniVar>,
    row_unification_table: InPlaceUnificationTable<RowUniVar>,

    partial_row_combs: BTreeSet<RowCombination>,
    row_to_combo: FxHashMap<NodeId, RowCombination>,
    branch_to_ret_ty: FxHashMap<NodeId, Spanned<Intern<Type>>>,

    subst_unifiers_to_tyvars: FxHashMap<TyUniVar, TypeVar>,
    next_tyvar: u32,
    subst_unifiers_to_rowvars: FxHashMap<RowUniVar, RowVar>,
    next_rowvar: u32,

    item_wrappers: FxHashMap<NodeId, ItemWrapper>,

    errors: FxHashMap<NodeId, CompilerErr>,
}

impl<'env> Solver<'env> {
    fn fresh_ty_var(&mut self) -> TyUniVar {
        self.tables.unification_table.new_key(None)
    }

    fn fresh_row_var(&mut self) -> RowUniVar {
        self.tables.row_unification_table.new_key(None)
    }

    fn fresh_row_combination(&mut self) -> RowCombination {
        RowCombination {
            left: Row::Unifier(self.fresh_row_var()),
            right: Row::Unifier(self.fresh_row_var()),
            goal: Row::Unifier(self.fresh_row_var()),
        }
    }

    fn tyvar_for_unifier(&mut self, var: TyUniVar) -> TypeVar {
        *self
            .tables
            .subst_unifiers_to_tyvars
            .entry(var)
            .or_insert_with(|| {
                let next = self.tables.next_tyvar;
                self.tables.next_tyvar += 1;

                // self.tables.hasher.write_u32(next);
                // let out = self.tables.hasher.finish().to_string().into();
                TypeVar(next.to_string().into())
            })
    }

    fn rowvar_for_unifier(&mut self, var: RowUniVar) -> RowVar {
        *self
            .tables
            .subst_unifiers_to_rowvars
            .entry(var)
            .or_insert_with(|| {
                let next = self.tables.next_rowvar;
                self.tables.next_rowvar += 1;
                RowVar(next)
            })
    }

    // pub fn type_infer(ast: Spanned<Intern<Expr<Untyped>>>) -> CompResult<TypeInferOut> {
    //     let ctx = Self{ default();
    //     ctx.type_infer_logic(ast)
    // }

    fn normalize_mentioned_row_combs<T>(
        &mut self,
        subst_out: SubstOut<T>,
    ) -> SubstOut<(T, Vec<Evidence>)> {
        let mut subst_out = subst_out.map(|t| (t, vec![]));
        for norm_row_comb in std::mem::take(&mut self.tables.partial_row_combs)
            .into_iter()
            .map(|row_comb| self.substitute_row_comb(row_comb))
        {
            if norm_row_comb
                .unbound_tys
                .intersection(&subst_out.unbound_tys)
                .next()
                .is_some()
                || norm_row_comb
                    .unbound_rows
                    .intersection(&subst_out.unbound_rows)
                    .next()
                    .is_some()
            {
                subst_out = subst_out.merge(norm_row_comb, |(t, mut evidences), ev| {
                    evidences.push(ev);
                    (t, evidences)
                })
            }
        }
        subst_out
    }

    pub fn type_infer_with_items(
        item_source: &'env ItemSource,
        ast: Spanned<Intern<Expr<Untyped>>>,
    ) -> Result<TypeInferOut, FxHashMap<NodeId, CompilerErr>> {
        let ctx = Self {
            item_source,
            tables: Default::default(),
        };
        ctx.type_infer_logic(ast)
    }

    pub fn type_infer_logic(
        mut self,
        ast: Spanned<Intern<Expr<Untyped>>>,
    ) -> Result<TypeInferOut, FxHashMap<NodeId, CompilerErr>> {
        // Constraint generation
        let (out, ty) = self.infer(im::HashMap::with_hasher(FxBuildHasher), ast);

        // Constraint solving

        if self.unification(out.constraints).is_err() {
            return Err(std::mem::take(&mut self.tables.errors));
        }; // Apply our substition to our inferred types
        let subst_out = self
            .substitute_ty(ty)
            .merge(self.substitute_ast(out.typed_ast), |ty, ast| (ty, ast));

        let mut evidence_subst = self.normalize_mentioned_row_combs(subst_out);
        let row_to_ev = std::mem::take(&mut self.tables.row_to_combo)
            .into_iter()
            .map(|(id, combo)| {
                let out = self.substitute_row_comb(combo);
                evidence_subst.unbound_rows.extend(out.unbound_rows);
                evidence_subst.unbound_tys.extend(out.unbound_tys);
                (id, out.value)
            })
            .collect();

        let item_wrappers = std::mem::take(&mut self.tables.item_wrappers)
            .into_iter()
            .map(|(id, wrapper)| {
                let out = self.substitute_wrapper(wrapper);
                evidence_subst.unbound_rows.extend(out.unbound_rows);
                evidence_subst.unbound_tys.extend(out.unbound_tys);
                (id, out.value)
            })
            .collect();

        let branch_to_ret_ty = std::mem::take(&mut self.tables.branch_to_ret_ty)
            .into_iter()
            .map(|(id, ty)| {
                let out = self.substitute_ty(ty);
                evidence_subst.unbound_rows.extend(out.unbound_rows);
                evidence_subst.unbound_tys.extend(out.unbound_tys);
                (id, out.value)
            })
            .collect();

        // Return our typed ast and it's type scheme
        let ((ty, ast), evidence) = evidence_subst.value;
        Ok(TypeInferOut {
            ast,
            errors: self.tables.errors,
            scheme: TypeScheme {
                unbound_rows: evidence_subst.unbound_rows,
                unbound_types: evidence_subst.unbound_tys,
                evidence,
                ty,
            },
            row_to_ev,
            branch_to_ret_ty,
            item_wrappers,
        })
    }

    pub fn check_with_items(
        item_source: &'env ItemSource,
        ast: Spanned<Intern<Expr<Untyped>>>,
        signature: TypeScheme,
    ) -> CompResult<TypesOutput> {
        let mut ctx = Self {
            item_source,

            tables: SolverTables {
                next_tyvar: (signature.unbound_types.iter().len() + 1) as u32,
                next_rowvar: signature
                    .unbound_rows
                    .iter()
                    .max()
                    .map(|rv| rv.0 + 1)
                    .unwrap_or(2),

                ..Default::default()
            },
        };
        ctx.type_check_logic(ast, signature)
    }

    fn type_check_logic(
        &mut self,
        ast: Spanned<Intern<Expr<Untyped>>>,
        signature: TypeScheme,
    ) -> CompResult<TypesOutput> {
        let id = ast.id();
        // We start with `check` instead of `infer`.
        let mut out = self.check(im::HashMap::default(), ast, signature.ty);

        // Add any evidence in our type annotation to be used during solving.
        out.constraints
            .extend(signature.evidence.iter().map(|ev| match *ev {
                Evidence::RowEquation { left, right, goal } => Constraint::RowCombine(
                    Provenance::ExpectedCombine(id),
                    RowCombination { left, right, goal },
                ),
            }));
        // dbg!(&out.constraints);
        if self.unification(out.constraints).is_err() {
            return Err(std::mem::take(&mut self.tables.errors)
                .into_values()
                .collect::<Vec<_>>()
                .into());
        };

        // We still need to substitute, but only our ast.
        let subst_out = self.substitute_ast(out.typed_ast);

        // Here we have to make sure we didn't invent new constraints or types
        // during unification, and if we did that's an error.
        let mut evidence_subst = self.normalize_mentioned_row_combs(subst_out);
        let row_to_ev = std::mem::take(&mut self.tables.row_to_combo)
            .into_iter()
            .map(|(id, combo)| {
                let out = self.substitute_row_comb(combo);
                evidence_subst.unbound_rows.extend(out.unbound_rows);
                evidence_subst.unbound_tys.extend(out.unbound_tys);
                (id, out.value)
            })
            .collect();
        let item_wrappers = std::mem::take(&mut self.tables.item_wrappers)
            .into_iter()
            .map(|(id, wrapper)| {
                let out = self.substitute_wrapper(wrapper);
                evidence_subst.unbound_rows.extend(out.unbound_rows);
                evidence_subst.unbound_tys.extend(out.unbound_tys);
                (id, out.value)
            })
            .collect();
        let branch_to_ret_ty = std::mem::take(&mut self.tables.branch_to_ret_ty)
            .into_iter()
            .map(|(id, ty)| {
                let out = self.substitute_ty(ty);
                evidence_subst.unbound_rows.extend(out.unbound_rows);
                evidence_subst.unbound_tys.extend(out.unbound_tys);
                (id, out.value)
            })
            .collect();
        let (typed_ast, evs) = evidence_subst.value;

        let extra_types = evidence_subst
            .unbound_tys
            .difference(&signature.unbound_types)
            .copied()
            .collect::<Vec<_>>();

        let extra_row = evidence_subst
            .unbound_rows
            .difference(&signature.unbound_rows)
            .copied()
            .collect::<Vec<_>>();

        let sig_evs = signature.evidence.iter().cloned().collect::<FxHashSet<_>>();
        let extra_evidence = evs
            .into_iter()
            .collect::<FxHashSet<_>>()
            .difference(&sig_evs)
            .cloned()
            .collect::<Vec<_>>();

        if !extra_types.is_empty() || !extra_row.is_empty() || !extra_evidence.is_empty() {
            return Err(
                DynamicErr::new("Checking introduced new constraints".to_string())
                    .help(format!("extra types: {extra_types:?}"))
                    .help(format!("extra rows: {extra_row:?}"))
                    .help(format!("extra evidence: {extra_evidence:#?}"))
                    .into(),
            );
        }

        Ok(TypesOutput {
            typed_ast,
            scheme: signature,
            errors: std::mem::take(&mut self.tables.errors),
            row_to_ev,
            branch_to_ret_ty,
            item_wrappers,
        })
    }
}
