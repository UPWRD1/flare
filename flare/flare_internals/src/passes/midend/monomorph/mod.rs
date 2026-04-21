use core::fmt;

use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    passes::midend::simplify,
    resource::rep::midend::{
        ir::{Branch, IR, ItemId},
        irtype::TyApp,
    },
};

// Monomorphisation — overview
// ===========================
//
// We work in three phases.
//
// Phase 1 — Discovery
//   Starting from main (the last item, which has no type parameters), we run
//   a worklist over (item, concrete_type_args) pairs called "Monomorphs".
//   For each new Monomorph we beta-reduce its leading TyFun binders against
//   the concrete type args (via simplify::subst_ty_at), which replaces every
//   type-variable reference in the body with a concrete type.  We then scan
//   the resulting body for TyApp-Item chains and bare Item calls to discover
//   the next generation of Monomorphs.
//
// Phase 2 — Ordering
//   We run an iterative post-order DFS over the dependency graph so that
//   every callee is assigned a lower ID than its callers.
//
// Phase 3 — Rewriting
//   For each Monomorph in post-order we beta-reduce its IR (same as Phase 1)
//   and then walk the result replacing every TyApp-Item chain or bare Item
//   with a plain Item carrying the new ID assigned in Phase 2.  TyFun nodes
//   are erased — monomorphic IR has no type abstractions.
//
// Why beta-reduction instead of depth-tracked substitution
// --------------------------------------------------------
//   simplify::subst_ty_at(body, t, 0) substitutes t for Var(0) in body and
//   correctly re-numbers all remaining De Bruijn indices.  Applying this once
//   per TyFun binder (always at depth 0, peeling from the outside in) makes
//   the substitution trivially correct without any manual depth arithmetic.

pub fn monomorph(the_ir: Vec<IR>) -> Vec<IR> {
    let main_id = ItemId(the_ir.len() - 1);

    let ref_ir: FxHashMap<ItemId, IR> = the_ir
        .into_iter()
        .enumerate()
        .map(|(i, ir)| (ItemId(i), ir))
        .collect();

    let root = Monomorph {
        ref_item: main_id,
        apps: Vec::new().leak(),
    };

    // Phase 1 — discover every reachable (item, type_args) pair.
    let all_deps = discover(&ref_ir, root);

    // Phase 2 — post-order: callees before callers.
    let order = post_order(root, &all_deps);
    let id_map: FxHashMap<Monomorph, ItemId> = order
        .iter()
        .enumerate()
        .map(|(i, m)| (*m, ItemId(i)))
        .collect();

    // Phase 3 — instantiate and rewrite.
    order
        .iter()
        .map(|mono| {
            let ir = ref_ir[&mono.ref_item].clone();
            let body = beta_reduce_tyfuns(ir, mono.apps);
            rewrite(body, &id_map)
        })
        // .inspect(|ir| println!("{ir}\n-------------------------------\n"))
        .collect()
}

// ── Core type ────────────────────────────────────────────────────────────────

/// A specific monomorphic instantiation of an IR item.
///
/// `apps` lists the concrete type arguments in application order (the first
/// element corresponds to the outermost `TyFun` binder).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Monomorph {
    ref_item: ItemId,
    apps: &'static [TyApp],
}

impl fmt::Display for Monomorph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}[{}]", self.ref_item.0, self.apps.iter().join(", "))
    }
}

// ── Phase 1: discovery ───────────────────────────────────────────────────────

/// Collect every Monomorph reachable from `root` together with its direct
/// dependencies, using a worklist so each Monomorph is processed only once.
fn discover(
    ref_ir: &FxHashMap<ItemId, IR>,
    root: Monomorph,
) -> FxHashMap<Monomorph, Vec<Monomorph>> {
    let mut all_deps: FxHashMap<Monomorph, Vec<Monomorph>> = FxHashMap::default();
    let mut worklist: Vec<Monomorph> = vec![root];

    while let Some(mono) = worklist.pop() {
        if all_deps.contains_key(&mono) {
            continue;
        }

        let ir = ref_ir[&mono.ref_item].clone();

        // After beta-reduction, every TyApp-Item chain in the body carries
        // fully concrete type arguments — no free type variables remain.
        let body = beta_reduce_tyfuns(ir, mono.apps);
        let deps = scan_calls(&body);

        for &dep in &deps {
            if !all_deps.contains_key(&dep) {
                worklist.push(dep);
            }
        }

        all_deps.insert(mono, deps);
    }

    all_deps
}

// ── Phase 2: ordering ────────────────────────────────────────────────────────

/// Iterative post-order DFS: every dependency appears before its dependents.
///
/// The `(node, children_pushed)` pair on the stack lets us emit a node only
/// after all of its children have been processed, without recursion.
fn post_order(root: Monomorph, all_deps: &FxHashMap<Monomorph, Vec<Monomorph>>) -> Vec<Monomorph> {
    let mut visited: FxHashSet<Monomorph> = FxHashSet::default();
    let mut order: Vec<Monomorph> = Vec::new();
    let mut stack: Vec<(Monomorph, bool)> = vec![(root, false)];

    while let Some((mono, children_pushed)) = stack.pop() {
        if children_pushed {
            // All descendants have been emitted; emit this node.
            order.push(mono);
            continue;
        }
        if visited.contains(&mono) {
            continue;
        }
        visited.insert(mono);

        // Re-push self so it is emitted after all children complete.
        stack.push((mono, true));

        // Push unvisited children.  Reversed so the first dependency is
        // processed first (preserves a natural left-to-right order).
        if let Some(deps) = all_deps.get(&mono) {
            for dep in deps.iter().rev() {
                if !visited.contains(dep) {
                    stack.push((*dep, false));
                }
            }
        }
    }

    order
}

// ── Shared helper: beta-reduction ────────────────────────────────────────────

/// Peel the leading `TyFun` binders off `ir`, substituting each concrete type
/// argument from `apps` in turn.
///
/// Each step performs one beta-reduction:
///   `(TyFun(_, body))[t]  →  subst_ty_at(body, t, 0)`
///
/// `subst_ty_at` replaces `Var(0)` throughout `body` with `t` and shifts all
/// remaining De Bruijn indices down by one, so repeated application is always
/// at depth 0 and index arithmetic stays trivial.
fn beta_reduce_tyfuns(mut ir: IR, apps: &[TyApp]) -> IR {
    for app in apps {
        ir = match ir {
            IR::TyFun(_, body) => match app {
                TyApp::Ty(t) => simplify::subst_ty_at(*body, t.clone(), 0),
                TyApp::Row(r) => simplify::subst_row_at(*body, r.clone(), 0),
            },
            // Fewer binders than type args: ill-typed input, stop early.
            other => panic!("NOt enough args"), //return other,
        };
    }
    ir
}

// ── Shared helper: TyApp chain peeling ───────────────────────────────────────

/// Walk a left-spine `TyApp` chain by reference and collect the base `Item` and
/// its type arguments in application order (outermost first).
///
/// `TyApp(TyApp(Item(f), A), B)`  →  `Some(Monomorph { f, [A, B] })`
///
/// Returns `None` if the base of the chain is not a bare `Item` (e.g. a
/// lambda or local variable).
fn peel_ty_app(ir: &IR) -> Option<Monomorph> {
    let mut apps: Vec<TyApp> = Vec::new();
    let mut cur = ir;
    loop {
        match cur {
            IR::TyApp(inner, app) => {
                apps.push(app.clone());
                cur = inner;
            }
            IR::Item(_, id) => {
                // Apps were accumulated outer→inner; reverse for canonical order.
                apps.reverse();
                return Some(Monomorph {
                    ref_item: *id,
                    apps: apps.leak(),
                });
            }
            _ => return None,
        }
    }
}

// ── Phase 1 helper: call scanning ────────────────────────────────────────────

/// Walk `ir` (after beta-reduction) with an explicit stack, collecting every
/// Item reference together with its type arguments as a `Monomorph`.
///
/// When a TyApp-Item chain is found it is peeled as a single unit.
/// Bare Item nodes (no type arguments) produce a Monomorph with empty apps.
/// Anything else is recursed into via `IR::children()`.
fn scan_calls(ir: &IR) -> Vec<Monomorph> {
    let mut result: Vec<Monomorph> = Vec::new();
    let mut stack: Vec<IR> = vec![ir.clone()];

    while let Some(node) = stack.pop() {
        match node {
            IR::TyApp(ref inner, _) => match peel_ty_app(&node) {
                Some(mono) => result.push(mono),
                // Chain doesn't end in a bare Item; descend into the body.
                None => stack.push(*inner.clone()),
            },
            IR::Item(_, id) => {
                result.push(Monomorph {
                    ref_item: id,
                    apps: Vec::new().leak(),
                });
            }
            node => {
                for child in node.children() {
                    stack.push(child);
                }
            }
        }
    }

    result
}

// ── Phase 3: rewriting ───────────────────────────────────────────────────────

/// Recursively rewrite `ir` to use the new monomorphic IDs from `id_map`.
///
/// - `TyFun` nodes are erased (monomorphic IR has no type abstractions).
/// - `TyApp`-`Item` chains are collapsed to a single `Item` with the new ID.
/// - Bare `Item` nodes are remapped to their new IDs.
/// - All other nodes are recursed into with their structure preserved.
fn rewrite(ir: IR, id_map: &FxHashMap<Monomorph, ItemId>) -> IR {
    match ir {
        // Type abstractions vanish in monomorphic IR.
        IR::TyFun(_, body) => rewrite(*body, id_map),

        // Try to resolve the whole TyApp-Item chain to a new bare Item.
        // Fall back to recursive rewriting if the chain is not a known Item.
        IR::TyApp(inner, app) => match resolve_ty_app_chain(&inner, &app, id_map) {
            Some(resolved) => resolved,
            None => IR::ty_app(rewrite(*inner, id_map), app),
        },

        // A bare Item called with no type arguments.
        IR::Item(t, id) => {
            let mono = Monomorph {
                ref_item: id,
                apps: Vec::new().leak(),
            };
            match id_map.get(&mono) {
                Some(&new_id) => IR::Item(t, new_id),
                None => IR::Item(t, id),
            }
        }

        IR::Local(v, d, b) => IR::local(v, rewrite(*d, id_map), rewrite(*b, id_map)),
        IR::App(l, r) => IR::app(rewrite(*l, id_map), rewrite(*r, id_map)),
        IR::Fun(v, body) => IR::fun(v, rewrite(*body, id_map)),
        IR::Tuple(v) => IR::Tuple(v.into_iter().map(|ir| rewrite(ir, id_map)).collect()),
        IR::Case(t, body, branches) => IR::case(
            t,
            rewrite(*body, id_map),
            branches.into_iter().map(|b| Branch {
                param: b.param,
                body: rewrite(b.body, id_map),
            }),
        ),
        IR::Field(ir, u) => IR::field(rewrite(*ir, id_map), u),
        IR::Tag(t, u, ir) => IR::tag(t, u, rewrite(*ir, id_map)),
        IR::If(c, t, e) => IR::If(
            Box::new(rewrite(*c, id_map)),
            Box::new(rewrite(*t, id_map)),
            Box::new(rewrite(*e, id_map)),
        ),
        IR::Bin(l, op, r) => IR::bin(rewrite(*l, id_map), op, rewrite(*r, id_map)),

        // Leaves with no Item references.
        IR::Extern(_, _)
        | IR::Num(_)
        | IR::Str(_)
        | IR::Bool(_)
        | IR::Unit
        | IR::Particle(_)
        | IR::Var(_) => ir,
    }
}

/// Peek into a `TyApp(inner, first_app)` chain *by reference*, build the
/// `Monomorph` key, and return the remapped `Item` if the chain ends in a
/// known Item.
///
/// Working by reference means we never consume the boxes before knowing
/// whether the lookup will succeed.  The caller falls back to plain recursive
/// rewriting when this returns `None`.
fn resolve_ty_app_chain(
    inner: &IR,
    first_app: &TyApp,
    id_map: &FxHashMap<Monomorph, ItemId>,
) -> Option<IR> {
    let mut apps: Vec<TyApp> = vec![first_app.clone()];
    let mut cur: &IR = inner;

    loop {
        match cur {
            IR::TyApp(inner2, new_app) => {
                apps.push(new_app.clone());
                cur = inner2;
            }
            IR::Item(og_ty, id) => {
                // Reverse to canonical application order before the lookup.
                apps.reverse();
                let mono = Monomorph {
                    ref_item: *id,
                    apps: apps.leak(),
                };
                let &new_id = id_map.get(&mono)?;
                // Apply the concrete type args to the item's type annotation.
                let new_ty = mono
                    .apps
                    .iter()
                    .fold(og_ty.clone(), |ty, a| ty.subst_app(a.clone()));
                // dbg!(mono.apps);
                // dbg!(og_ty, &new_ty);
                return Some(IR::Item(new_ty, new_id));
            }
            // Base is not an Item; signal the caller to fall back.
            _ => return None,
        }
    }
}
