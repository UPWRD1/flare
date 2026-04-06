mod check;
mod infer;
mod inst;
mod rows;
mod subst;
mod types;
mod unify;

pub use rows::{ClosedRow, Row, RowVar};
use rows::{RowCombination, RowUniVar};
pub use types::{TyUniVar, Type, TypeVar};

use std::{collections::BTreeSet, fmt::Display, hash::Hash};

use ena::unify::InPlaceUnificationTable;
use internment::Intern;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    passes::frontend::typing::subst::SubstOut,
    resource::{
        errors::{CompResult, CompilerErr, DynamicErr},
        rep::{
            common::{FlareSpan, HasSpan, Spanned, Variable},
            frontend::ast::{Expr, ItemId, Kind, Label, Untyped},
        },
    },
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Typed(pub Untyped, pub Spanned<Intern<Type>>);

impl Variable for Typed {}

impl HasSpan for Typed {
    fn span(&self) -> FlareSpan {
        self.0.span()
    }
}

impl Display for Typed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Constraint {
    TypeEqual(Provenance, Spanned<Intern<Type>>, Spanned<Intern<Type>>),
    RowCombine(Provenance, RowCombination),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Evidence {
    RowEquation {
        left: Spanned<Intern<Row>>,
        right: Spanned<Intern<Row>>,
        goal: Spanned<Intern<Row>>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum Provenance {
    // A non function type encountered a Fun ast node, causing a type mismatch.
    UnexpectedFun(FlareSpan),
    // An application has an ast node in function position that does not have a function type.
    AppExpectedFun(FlareSpan),
    // Constraint produced by subsumption.
    ExpectedUnify(FlareSpan, FlareSpan),

    ExpectedCombine(FlareSpan, FlareSpan),

    ConditionIsBool(FlareSpan),
    FieldAccess(FlareSpan, Label),
}

impl Provenance {
    fn id(&self) -> FlareSpan {
        match self {
            Self::UnexpectedFun(node_id)
            | Self::AppExpectedFun(node_id)
            | Self::ExpectedCombine(node_id, _)
            | Self::ConditionIsBool(node_id)
            | Self::ExpectedUnify(node_id, _)
            | Self::FieldAccess(node_id, _) => *node_id,
        }
    }
    fn to_dyn_err(self, left: &Spanned<String>, right: &Spanned<String>) -> DynamicErr {
        // let left = l
        match self {
            Self::UnexpectedFun(node_id) => {
                DynamicErr::new("Encountered an unexpected function".to_string())
                    .label("This is a function", node_id)
                    .extra(format!("This is {}", left.0), left.1)
                    .extra(format!("This is {}", right.0), right.1)
            }
            Self::AppExpectedFun(node_id) => DynamicErr::new("Expected a function".to_string())
                .label("This is not a function", node_id)
                .extra(format!("This is {}", left.0), left.1)
                .extra(format!("This is {}", right.0), right.1),
            Self::ExpectedUnify(simple_span, simple_span1) => {
                DynamicErr::new(format!("Type mismatch between {} and {}", left.0, right.0))
                    .label(format!("This is {}", left.0), left.1)
                    .extra(format!("This is {}", right.0), right.1)
            }
            Self::ExpectedCombine(l_span, r_span) => {
                DynamicErr::new(format!("Row mismatch between {} and {}", left.0, right.0))
                    .label(
                        format!("Expected {} to combine with {}", right.0, left.0),
                        l_span,
                    )
                    .extra("and here", r_span)
            }
            Self::ConditionIsBool(simple_span) => DynamicErr::new("Expected bool").label(
                format!("This condition should be a bool, found {}", right.0),
                right.1,
            ),
            Self::FieldAccess(simple_span, label) => todo!(),
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
}

#[derive(PartialEq, Eq, Clone, Debug, Default, Hash)]
pub struct TypeScheme {
    pub unbound_types: BTreeSet<TypeVar>,
    pub unbound_rows: BTreeSet<RowVar>,
    pub evidence: Vec<Evidence>,
    pub ty: Spanned<Intern<Type>>,
    pub types_to_name: Vec<(TypeVar, Intern<String>)>,
    pub kind: Kind,
}

impl TypeScheme {
    pub fn merge(mut self, mut rhs: Self) -> Self {
        self.unbound_types.append(&mut rhs.unbound_types);
        self.unbound_rows.append(&mut rhs.unbound_rows);
        self.evidence.append(&mut rhs.evidence);
        self.types_to_name.append(&mut rhs.types_to_name);
        self
    }
}

impl HasSpan for TypeScheme {
    fn span(&self) -> FlareSpan {
        self.ty.span()
    }
}

impl TypeScheme {
    pub fn new(kind: Kind) -> Self {
        Self {
            kind,
            ..Default::default()
        }
    }
}

#[derive(Debug)]
pub struct TypeInferOut {
    pub ast: Spanned<Intern<Expr<Typed>>>,
    pub scheme: TypeScheme,
    pub errors: FxHashMap<FlareSpan, CompilerErr>,
    pub row_to_ev: FxHashMap<FlareSpan, Evidence>,
    pub branch_to_ret_ty: FxHashMap<FlareSpan, Spanned<Intern<Type>>>,
    pub item_wrappers: FxHashMap<FlareSpan, ItemWrapper>,
}

impl TypeInferOut {
    pub fn to_typesoutput(self) -> TypesOutput {
        TypesOutput {
            typed_ast: self.ast,
            scheme: self.scheme,
            errors: self.errors,
            row_to_ev: self.row_to_ev,
            branch_to_ret_ty: self.branch_to_ret_ty,
            item_wrappers: self.item_wrappers,
        }
    }
}

#[derive(Debug)]
pub struct TypesOutput {
    pub typed_ast: Spanned<Intern<Expr<Typed>>>,
    pub scheme: TypeScheme,
    pub errors: FxHashMap<FlareSpan, CompilerErr>,
    pub row_to_ev: FxHashMap<FlareSpan, Evidence>,
    pub branch_to_ret_ty: FxHashMap<FlareSpan, Spanned<Intern<Type>>>,
    pub item_wrappers: FxHashMap<FlareSpan, ItemWrapper>,
}

#[derive(Debug, Clone)]
pub struct ItemWrapper {
    pub types: Vec<Spanned<Intern<Type>>>,
    pub rows: Vec<Spanned<Intern<Row>>>,
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
        // dbg!item_id);
        // d cbg!(self);
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
    row_to_combo: FxHashMap<FlareSpan, RowCombination>,
    branch_to_ret_ty: FxHashMap<FlareSpan, Spanned<Intern<Type>>>,

    subst_unifiers_to_tyvars: FxHashMap<TyUniVar, TypeVar>,
    next_tyvar: usize,
    subst_unifiers_to_rowvars: FxHashMap<RowUniVar, RowVar>,
    next_rowvar: usize,

    item_wrappers: FxHashMap<FlareSpan, ItemWrapper>,

    errors: FxHashMap<FlareSpan, CompilerErr>,
}

impl<'env> Solver<'env> {
    fn fresh_ty_var(&mut self) -> TyUniVar {
        self.tables.unification_table.new_key(None)
    }

    fn fresh_row_var(&mut self) -> RowUniVar {
        self.tables.row_unification_table.new_key(None)
    }

    fn fresh_row_combination(
        &mut self,
        l_id: FlareSpan,
        r_id: FlareSpan,
        goal_id: FlareSpan,
    ) -> RowCombination {
        RowCombination {
            left: Spanned(Row::Unifier(self.fresh_row_var()).into(), l_id),
            right: Spanned(Row::Unifier(self.fresh_row_var()).into(), r_id),
            goal: Spanned(Row::Unifier(self.fresh_row_var()).into(), goal_id),
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

                TypeVar(next)
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
                RowVar(next.to_string().into())
            })
    }

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

    pub fn check_with_items(
        item_source: &'env ItemSource,
        ast: Spanned<Intern<Expr<Untyped>>>,
        signature: TypeScheme,
    ) -> CompResult<TypesOutput> {
        let mut ctx = Self {
            item_source,

            tables: SolverTables {
                next_tyvar: (signature.unbound_types.iter().len() + 1),
                next_rowvar: (signature.unbound_rows.iter().len() + 1),
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
        // dbg!(&signature);
        // let id = ast.id();
        // We start with `check` instead of `infer`.
        let mut out = self.check(im::HashMap::default(), ast, signature.ty);

        // Add any evidence in our type annotation to be used during solving.
        out.constraints
            .extend(signature.evidence.iter().map(|ev| match *ev {
                Evidence::RowEquation { left, right, goal } => Constraint::RowCombine(
                    Provenance::ExpectedCombine(left.1, right.1),
                    RowCombination { left, right, goal },
                ),
            }));
        // dbg!(&out.constraints);
        if self.unification(out.constraints, &signature).is_err() {
            return Err(std::mem::take(&mut self.tables.errors)
                .into_values()
                .collect::<Vec<_>>()
                .into());
        }

        // We still need to substitute, but only our ast.
        let subst_out = self.substitute_ast(out.typed_ast);
        // dbg!(&self.tables.row_to_combo);
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

        let sig_evs = signature.evidence.iter().copied().collect::<FxHashSet<_>>();
        let extra_evidence = evs
            .into_iter()
            .collect::<FxHashSet<_>>()
            .difference(&sig_evs)
            .copied()
            .collect::<Vec<_>>();

        if !extra_types.is_empty() || !extra_row.is_empty() || !extra_evidence.is_empty() {
            return Err(
                DynamicErr::new("Checking introduced new constraints".to_string())
                    .help(format!("extra types: {extra_types:?}"))
                    .help(format!("extra rows: {extra_row:?}"))
                    .help(format!("extra evidence: {extra_evidence:#?}"))
                    .help(format!("signature: {signature:#?}"))
                    .into(),
            );
        }
        // dbg!(self.normalize_ty(signature.ty));

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
