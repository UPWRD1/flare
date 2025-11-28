mod check;
mod infer;
mod rows;
mod subst;
mod types;
mod unify;

pub use rows::{ClosedRow, Row};
use rows::{RowCombination, RowUniVar};
pub use types::{TyUniVar, Type, TypeVar};

use std::{
    collections::BTreeSet,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use ena::unify::InPlaceUnificationTable;
use internment::Intern;

use rustc_hash::{FxHashMap, FxHasher};

use crate::{
    passes::midend::{
        environment::Environment,
        typing::{rows::RowVar, subst::SubstOut},
    },
    resource::{
        errors::{CompResult, CompilerErr, TypeErr},
        rep::{
            ast::{Direction, Expr, ItemId, NodeId, Untyped, Variable},
            common::Ident,
            entry::Item,
            quantifier::QualifierFragment,
            Spanned,
        },
    },
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Typed(pub Untyped, pub Type);

impl Variable for Typed {}

impl Ident for Typed {
    fn ident(&self) -> CompResult<Spanned<Intern<String>>> {
        self.0.ident()
    }
}

// #[derive(Debug, Clone, Copy)]
enum Constraint {
    TypeEqual(Provenance, Type, Type),
    RowCombine(RowCombination),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Evidence {
    RowEquation { left: Row, right: Row, goal: Row },
}

#[derive(Debug, Clone, Copy)]
enum Provenance {
    // A non function type encountered a Fun ast node, causing a type mismatch.
    UnexpectedFun(NodeId),
    // An application has an ast node in function position that does not have a function type.
    AppExpectedFun(NodeId),
    // Constraint produced by subsumption.
    ExpectedUnify(NodeId),
}

impl Provenance {
    fn id(&self) -> NodeId {
        match self {
            Self::UnexpectedFun(node_id)
            | Self::AppExpectedFun(node_id)
            | Self::ExpectedUnify(node_id) => *node_id,
        }
    }
}

pub struct GenOut {
    constraints: Vec<Constraint>,
    typed_ast: Spanned<Intern<Expr<Typed>>>,
}

impl GenOut {
    fn new(constraints: Vec<Constraint>, typed_ast: Spanned<Intern<Expr<Typed>>>) -> Self {
        Self {
            constraints,
            typed_ast,
        }
    }

    fn with_typed_ast(
        self,
        f: impl FnOnce(Spanned<Intern<Expr<Typed>>>) -> Spanned<Intern<Expr<Typed>>>,
    ) -> Self {
        Self {
            constraints: self.constraints,
            typed_ast: f(self.typed_ast),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TypeScheme {
    pub unbound_types: BTreeSet<TypeVar>,

    pub unbound_rows: BTreeSet<RowVar>,
    pub evidence: Vec<Evidence>,
    pub ty: Type,
}

pub struct TypeInferOut {
    pub ast: Spanned<Intern<Expr<Typed>>>,
    pub scheme: TypeScheme,
    pub errors: FxHashMap<NodeId, CompilerErr>,
    pub row_to_ev: FxHashMap<NodeId, Evidence>,
    pub branch_to_ret_ty: FxHashMap<NodeId, Type>,
    pub item_wrappers: FxHashMap<NodeId, ItemWrapper>,
}

struct ItemWrapper {
    types: Vec<Type>,
    rows: Vec<Row>,
    evidence: Vec<Evidence>,
}

#[derive(Default)]
struct ItemSource {
    types: FxHashMap<ItemId, TypeScheme>,
}

#[derive(Default)]
pub struct Solver<'env> {
    unification_table: InPlaceUnificationTable<TyUniVar>,
    row_unification_table: InPlaceUnificationTable<RowUniVar>,

    hasher: FxHasher,

    partial_row_combs: BTreeSet<RowCombination>,
    row_to_combo: FxHashMap<NodeId, RowCombination>,
    branch_to_ret_ty: FxHashMap<NodeId, Type>,

    subst_unifiers_to_tyvars: FxHashMap<TyUniVar, TypeVar>,
    next_tyvar: u32,
    subst_unifiers_to_rowvars: FxHashMap<RowUniVar, RowVar>,
    next_rowvar: u32,

    item_wrappers: FxHashMap<NodeId, ItemWrapper>,

    phantom: PhantomData<&'env Environment>,
    errors: FxHashMap<NodeId, CompilerErr>,
    item_source: ItemSource,
}

impl<'env> Solver<'env> {
    fn fresh_ty_var(&mut self) -> TyUniVar {
        self.unification_table.new_key(None)
    }

    fn fresh_row_var(&mut self) -> RowUniVar {
        self.row_unification_table.new_key(None)
    }

    fn fresh_row_combination(&mut self) -> RowCombination {
        RowCombination {
            left: Row::Unifier(self.fresh_row_var()),
            right: Row::Unifier(self.fresh_row_var()),
            goal: Row::Unifier(self.fresh_row_var()),
        }
    }

    fn tyvar_for_unifier(&mut self, var: TyUniVar) -> TypeVar {
        *self.subst_unifiers_to_tyvars.entry(var).or_insert_with(|| {
            let next = self.next_tyvar;
            self.next_tyvar += 1;
            self.hasher.write_u32(next);
            let out = self.hasher.finish().to_string().into();
            TypeVar(out)
        })
    }

    fn rowvar_for_unifier(&mut self, var: RowUniVar) -> RowVar {
        *self
            .subst_unifiers_to_rowvars
            .entry(var)
            .or_insert_with(|| {
                let next = self.next_rowvar;
                self.next_rowvar += 1;
                RowVar(next)
            })
    }

    pub fn type_infer(ast: Spanned<Intern<Expr<Untyped>>>) -> CompResult<TypeInferOut> {
        let ctx = Self::default();
        ctx.type_infer_logic(ast)
    }

    fn normalize_mentioned_row_combs<T>(
        &mut self,
        subst_out: SubstOut<T>,
    ) -> SubstOut<(T, Vec<Evidence>)> {
        let mut subst_out = subst_out.map(|t| (t, vec![]));
        for norm_row_comb in std::mem::take(&mut self.partial_row_combs)
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
        item_source: ItemSource,
        ast: Spanned<Intern<Expr<Untyped>>>,
    ) -> CompResult<TypeInferOut> {
        let ctx = Self {
            item_source,
            ..Default::default()
        };
        ctx.type_infer_logic(ast)
    }

    pub fn type_infer_logic(
        mut self,
        ast: Spanned<Intern<Expr<Untyped>>>,
    ) -> CompResult<TypeInferOut> {
        // Constraint generation
        let (out, ty) = self.infer(im::HashMap::default(), ast);

        // Constraint solving
        self.unification(out.constraints)?;

        // Apply our substition to our inferred types
        let subst_out = self
            .substitute_ty(ty)
            .merge(self.substitute_ast(out.typed_ast), |ty, ast| (ty, ast));

        let mut evidence_subst = self.normalize_mentioned_row_combs(subst_out);
        let row_to_ev = std::mem::take(&mut self.row_to_combo)
            .into_iter()
            .map(|(id, combo)| {
                let out = self.substitute_row_comb(combo);
                evidence_subst.unbound_rows.extend(out.unbound_rows);
                evidence_subst.unbound_tys.extend(out.unbound_tys);
                (id, out.value)
            })
            .collect();
        let item_wrappers = std::mem::take(&mut self.item_wrappers)
            .into_iter()
            .map(|(id, wrapper)| {
                let out = self.substitute_wrapper(wrapper);
                evidence_subst.unbound_rows.extend(out.unbound_rows);
                evidence_subst.unbound_tys.extend(out.unbound_tys);
                (id, out.value)
            })
            .collect();
        let branch_to_ret_ty = std::mem::take(&mut self.branch_to_ret_ty)
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
            errors: self.errors,
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

    fn type_check(
        ast: Spanned<Intern<Expr<Untyped>>>,
        signature: TypeScheme,
    ) -> CompResult<Spanned<Intern<Expr<Typed>>>> {
        let mut ctx = Self::default();
        ctx.type_check_logic(ast, signature)
    }

    fn type_check_with_items(
        item_source: ItemSource,
        ast: Spanned<Intern<Expr<Untyped>>>,
        signature: TypeScheme,
    ) -> CompResult<Spanned<Intern<Expr<Typed>>>> {
        let mut ctx = Self {
            item_source,
            ..Default::default()
        };
        ctx.type_check_logic(ast, signature)
    }

    fn type_check_logic(
        &mut self,
        ast: Spanned<Intern<Expr<Untyped>>>,
        signature: TypeScheme,
    ) -> CompResult<Spanned<Intern<Expr<Typed>>>> {
        // We start with `check` instead of `infer`.
        let mut out = self.check(im::HashMap::default(), ast, signature.ty);

        // Add any evidence in our type annotation to be used during solving.
        out.constraints
            .extend(signature.evidence.iter().map(|ev| match *ev {
                Evidence::RowEquation { left, right, goal } => {
                    Constraint::RowCombine(RowCombination { left, right, goal })
                }
            }));

        self.unification(out.constraints)?;

        // We still need to substitute, but only our ast.
        let subst_ast = self.substitute_ast(out.typed_ast);

        // And we're done
        Ok(subst_ast.value)
    }
    /// Check a single item from the environment.
    fn check_item(
        &mut self,
        item: &'env Item,
        packctx: QualifierFragment,
    ) -> CompResult<&'env Item> {
        todo!()
    }
}
