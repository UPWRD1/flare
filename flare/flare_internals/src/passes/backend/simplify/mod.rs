use std::ops::ControlFlow;

use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};

use crate::passes::backend::lowering::ir::{Branch, IR, Kind, TyApp, Type, Var, VarId};

enum Param {
    Ty(Kind),
    Val(Var),
}

enum Arg<'a> {
    Val(&'a IR),
    Ty(&'a TyApp),
}

impl IR {
    fn size(&self) -> usize {
        fn size_app(ir: &IR, args: usize) -> usize {
            match ir {
                IR::App(fun, arg) => arg.size() + size_app(fun, args + 1),
                IR::TyApp(ir, _) => size_app(ir, args),
                ir => ir.size() + 10 * (1 + args),
            }
        }
        match self {
            Self::Var(_)
            | Self::Num(_)
            | Self::Str(_)
            | Self::Bool(_)
            | Self::Unit
            | Self::Particle(_) => 0,

            // Arith is always cheap
            // Self::Add(_, _) | Self::Sub(_, _) | Self::Mul(_, _) | Self::Div(_, _) => 0,
            Self::Fun(_, body) => 10 + body.size(),
            Self::App(fun, arg) => arg.size() + size_app(fun, 1),
            Self::TyFun(_, ir) => ir.size(),
            Self::TyApp(ir, _) => ir.size(),
            Self::Local(var, defn, body) => {
                defn.size() + body.size() + (if var.ty.is_cheap_alloc() { 0 } else { 10 })
            }

            _ => todo!(),
        }
    }
    fn is_trivial(&self) -> bool {
        matches!(
            self,
            Self::Var(_)
                | Self::Num(_)
                | Self::Str(_)
                | Self::Unit
                | Self::Bool(_)
                | Self::Particle(_)
        )
    }
    pub fn is_value(&self) -> bool {
        match self {
            Self::Var(_)
            | Self::Num(_)
            | Self::Str(_)
            | Self::Unit
            | Self::Bool(_)
            | Self::Particle(_)
            | Self::Fun(_, _) => true,
            Self::TyFun(_, ir) => ir.is_value(),
            Self::TyApp(ir, _) => ir.is_value(),
            Self::Local(_, defn, body) => defn.is_value() && body.is_value(),
            Self::Tuple(s) => s.iter().all(|x| x.is_value()),
            // Self::Tag(_, _, ir) =
            // Self::Add(_, _)
            // | Self::Sub(_, _)
            // | Self::Mul(_, _)
            // | Self::Div(_, _)
            // | Self::App(_, _) => false,
            _ => todo!("{self:?}"),
            // _ => false,
        }
    }

    fn split_funs(self) -> (Vec<Param>, Self) {
        fn split_funs(ir: IR, params: &mut Vec<Param>) -> IR {
            match ir {
                IR::TyFun(kind, ir) => {
                    params.push(Param::Ty(kind));
                    split_funs(*ir, params)
                }
                IR::Fun(var, ir) => {
                    params.push(Param::Val(var));
                    split_funs(*ir, params)
                }
                ir => ir,
            }
        }
        let mut params = vec![];
        let body = split_funs(self, &mut params);
        (params, body)
    }
}

#[derive(Debug, Clone, Copy)]
enum Occurrence {
    /// Never occurs
    Dead,
    /// Appears once
    Once,
    /// Appears once inside a function
    OnceInFun,
    /// Appears many times
    Many,
}

#[derive(Default, Debug)]
struct Occurrences {
    vars: FxHashMap<VarId, Occurrence>,
}

impl Occurrences {
    fn with_var_once(var: VarId) -> Self {
        let mut vars = FxHashMap::default();
        vars.insert(var, Occurrence::Once);
        Self { vars }
        // Self {
        // vars: FxHashMap::from_iter([(v, Occurrence::Once)]),
        // }
    }

    fn lookup_var(&self, k: &Var) -> Occurrence {
        *self.vars.get(&k.id).unwrap_or_else(|| {
            unreachable!(
                "Tried to reference undefined variable during simplification: {k:?}, \n\n{self:?}",
            )
        })
    }

    fn mark_dead(&mut self, k: VarId) {
        // self.vars.remove(&k);
        self.vars.entry(k).and_modify(|x| *x = Occurrence::Dead);
    }

    fn in_fun(self, free: &FxHashSet<VarId>) -> Self {
        Self {
            vars: self
                .vars
                .into_iter()
                .map(|(id, occ)| {
                    (
                        id,
                        match occ {
                            Occurrence::Once if free.contains(&id) => Occurrence::OnceInFun,
                            occ => occ,
                        },
                    )
                })
                .collect(),
        }
    }

    #[must_use]
    fn merge(mut self, other: Self) -> Self {
        for (var, occ) in other.vars {
            self.vars
                .entry(var)
                .and_modify(|self_occ| {
                    *self_occ = match (*self_occ, occ) {
                        (Occurrence::Dead, occ) | (occ, Occurrence::Dead) => occ,
                        (Occurrence::Many, _) | (_, Occurrence::Many) => Occurrence::Many,
                        (Occurrence::Once, Occurrence::Once) => Occurrence::Many,
                        (Occurrence::Once, _) | (Occurrence::OnceInFun, _) => Occurrence::Many,
                    };
                })
                .or_insert(occ);
        }
        self
    }
}

fn subst_ty(haystack: IR, payload: Type) -> IR {
    match haystack {
        IR::Var(var) => IR::Var(var.map_ty(|ty| ty.subst_ty(payload))),
        IR::Num(i) => IR::Num(i),
        IR::Str(s) => IR::Str(s),
        IR::Bool(b) => IR::Bool(b),
        IR::Unit => IR::Unit,
        IR::Particle(p) => IR::Particle(p),

        IR::Fun(var, ir) => IR::fun(
            var.map_ty(|ty| ty.subst_ty(payload.clone())),
            subst_ty(*ir, payload),
        ),
        IR::App(fun, arg) => IR::app(subst_ty(*fun, payload.clone()), subst_ty(*arg, payload)),
        IR::TyFun(kind, ir) => IR::ty_fun(kind, subst_ty(*ir, payload)),
        IR::TyApp(ir, ty) => IR::ty_app(
            subst_ty(*ir, payload.clone()),
            ty.subst_tyapp(TyApp::Ty(payload)),
        ),
        IR::Local(var, defn, body) => IR::local(
            var.map_ty(|ty| ty.subst_ty(payload.clone())),
            subst_ty(*defn, payload.clone()),
            subst_ty(*body, payload),
        ),

        _ => todo!(),
    }
}
fn occurrence_analysis(ir: &IR) -> (FxHashSet<VarId>, Occurrences) {
    match ir {
        IR::Var(var) => {
            let mut free = FxHashSet::default();
            free.insert(var.id);
            (free, Occurrences::with_var_once(var.id))
        }
        IR::Num(_) | IR::Str(_) | IR::Bool(_) | IR::Unit | IR::Particle(_) => {
            (FxHashSet::default(), Occurrences::default())
        }
        IR::Fun(var, ir) => {
            let (mut free, occs) = occurrence_analysis(ir);
            free.remove(&var.id);
            let occs = occs.in_fun(&free);
            (free, occs)
        }

        IR::App(fun, arg) => {
            let (mut fun_free, fun_occs) = occurrence_analysis(fun);
            let (arg_free, arg_occs) = occurrence_analysis(arg);
            fun_free.extend(arg_free);
            (fun_free, fun_occs.merge(arg_occs))
        }
        IR::TyFun(_, ir) => occurrence_analysis(ir),
        IR::TyApp(ir, _) => occurrence_analysis(ir),
        IR::Local(var, defn, body) => {
            let (mut free, occs) = occurrence_analysis(body);
            let (defn_free, defn_occs) = occurrence_analysis(defn);
            free.extend(defn_free);
            free.remove(&var.id);
            (free, defn_occs.merge(occs))
        }
        IR::Field(ir, _) => occurrence_analysis(ir),
        IR::Tuple(elements) => {
            let mut occs = Occurrences::default();
            let mut free = FxHashSet::default();
            for elem in elements {
                let (elem_free, elem_occs) = occurrence_analysis(elem);
                free.extend(elem_free);
                occs = occs.merge(elem_occs)
            }
            (free, occs)
        }
        IR::Case(_, scrutinee, branches) => {
            let (mut scrutinee_free, mut scrutinee_occs) = occurrence_analysis(scrutinee);
            for branch in branches {
                let (branch_free, branch_occs) = occurrence_analysis(&branch.as_fun());
                scrutinee_free.extend(branch_free);
                scrutinee_occs = scrutinee_occs.merge(branch_occs)
            }
            (scrutinee_free, scrutinee_occs)
        }
        IR::Tag(_, _, ir) => occurrence_analysis(ir),
        IR::Item(_, _) | IR::Extern(_, _) => (Default::default(), Default::default()),
        _ => todo!("{ir:?}"),
    }
}

type Subst = FxHashMap<VarId, SubstRng>;
#[derive(Debug, Clone)]
enum SubstRng {
    Suspend(IR, Subst),
    Done(IR),
}

type InScope = im::HashMap<VarId, Definition, FxBuildHasher>;

#[derive(Debug, Clone)]
enum Definition {
    Unknown,
    BoundTo(IR, Occurrence),
}

type SimplifierContext = Vec<(ContextEntry, Subst)>;

enum ContextEntry {
    App(IR),
    TyApp(TyApp),
    TyFun(Kind),
    Local(Var, Occurrence, IR),
    Field(usize),
    Tag(Type, usize),
    Case(Type, Vec<Branch>),
}

#[derive(Default)]
struct Simplifier {
    occs: Occurrences,
    subst: Subst,
    saturated_fun_count: usize,
    saturated_ty_fun_count: usize,
    locals_inlined: usize,
    inline_size_threshold: usize,
}

pub fn simplify(ir: Vec<(IR, Type)>) -> Vec<(IR, Type)> {
    ir.into_iter()
        .map(|(mut ir, t)| {
            for _ in 0..2 {
                let (_, occs) = occurrence_analysis(&ir);
                let mut simplifier = Simplifier::new(occs);
                ir = simplifier.simplify(ir, InScope::default(), vec![]);
                if simplifier.did_no_work() {
                    break;
                }
            }
            (ir, t)
        })
        .collect()
}

impl Simplifier {
    fn new(occs: Occurrences) -> Self {
        Self {
            occs,
            inline_size_threshold: 60,
            ..Default::default()
        }
    }
    fn did_no_work(&self) -> bool {
        self.saturated_fun_count == 0
            && self.saturated_ty_fun_count == 0
            && self.locals_inlined == 0
    }

    fn simplify(&mut self, mut ir: IR, in_scope: InScope, mut ctx: SimplifierContext) -> IR {
        loop {
            // dbg!(&ir);
            ir = match ir {
                IR::App(fun, arg) => {
                    ctx.push((ContextEntry::App(*arg), self.subst.clone()));
                    *fun
                }
                IR::TyApp(ty_fun, ty_app) => {
                    ctx.push((ContextEntry::TyApp(ty_app), self.subst.clone()));
                    *ty_fun
                }
                IR::TyFun(kind, body) => {
                    ctx.push((ContextEntry::TyFun(kind), self.subst.clone()));
                    *body
                }
                IR::Num(i) => break self.rebuild(IR::Num(i), in_scope, ctx),
                IR::Str(i) => break self.rebuild(IR::Str(i), in_scope, ctx),
                IR::Bool(b) => break self.rebuild(IR::Bool(b), in_scope, ctx),
                IR::Unit => break self.rebuild(IR::Unit, in_scope, ctx),
                IR::Particle(p) => break self.rebuild(IR::Particle(p), in_scope, ctx),

                IR::Fun(var, body) => {
                    let body =
                        self.simplify(*body, in_scope.update(var.id, Definition::Unknown), vec![]);
                    break self.rebuild(IR::fun(var, body), in_scope, ctx);
                }
                IR::Local(var, defn, body) => self.simplify_local(var, *defn, *body, &mut ctx),
                IR::Var(var) => match self.simplify_var(var, in_scope.clone(), &ctx) {
                    ControlFlow::Continue(ir) => ir,
                    ControlFlow::Break(var) => break self.rebuild(IR::Var(var), in_scope, ctx),
                },

                IR::Tuple(elements) => {
                    let new_elements = elements
                        .into_iter()
                        .map(|elem| self.simplify(elem, in_scope.clone(), vec![]))
                        .collect();
                    break self.rebuild(IR::Tuple(new_elements), in_scope, ctx);
                }
                IR::Field(obj, idx) => {
                    ctx.push((ContextEntry::Field(idx), self.subst.clone()));
                    *obj
                }
                IR::Case(ty, scrutinee, branches) => {
                    let branches: Vec<Branch> = branches
                        .into_iter()
                        .map(|b| self.simplify_branch(b, in_scope.clone(), vec![]))
                        .collect();

                    ctx.push((ContextEntry::Case(ty, branches), self.subst.clone()));
                    *scrutinee
                }

                IR::Tag(ty, idx, ir) => {
                    // ctx.push((ContextEntry::Tag(ty, idx), self.subst.clone()));
                    break self.rebuild(*ir, in_scope, ctx);
                    // *ir
                }
                ir @ IR::Extern(_, _) | ir @ IR::Item(_, _) => {
                    break self.rebuild(ir, in_scope, ctx);
                }
            }
        }
    }

    fn simplify_branch(&mut self, b: Branch, in_scope: InScope, ctx: SimplifierContext) -> Branch {
        Branch {
            body: self.simplify(
                b.body,
                in_scope.update(b.param.id, Definition::Unknown),
                ctx,
            ),
            ..b
        }
    }

    fn simplify_local(&mut self, var: Var, defn: IR, body: IR, ctx: &mut SimplifierContext) -> IR {
        match self.occs.lookup_var(&var) {
            Occurrence::Dead => {
                self.locals_inlined += 1;
                body
            }
            Occurrence::Once => {
                self.locals_inlined += 1;
                let subst = self.subst.clone();
                self.subst.insert(var.id, SubstRng::Suspend(defn, subst));
                body
            }
            occ => {
                ctx.push((ContextEntry::Local(var, occ, body), self.subst.clone()));
                defn
            }
        }
    }

    fn simplify_var(
        &mut self,
        var: Var,
        in_scope: InScope,
        ctx: &SimplifierContext,
    ) -> ControlFlow<Var, IR> {
        match self.subst.remove(&var.id) {
            Some(SubstRng::Suspend(payload, subst)) => {
                self.subst = subst;
                ControlFlow::Continue(payload)
            }
            Some(SubstRng::Done(payload)) => {
                self.subst = Subst::default();
                ControlFlow::Continue(payload)
            }
            None => self.callsite_inline(var, in_scope, ctx),
        }
    }

    fn callsite_inline(
        &mut self,
        var: Var,
        in_scope: InScope,
        ctx: &SimplifierContext,
    ) -> ControlFlow<Var, IR> {
        in_scope
            .get(&var.id)
            .map(|bind| match bind {
                Definition::BoundTo(definition, occ)
                    if self.should_inline(definition, *occ, &in_scope, ctx) =>
                {
                    self.subst = Subst::default();
                    if let Occurrence::OnceInFun = occ {
                        self.occs.mark_dead(var.id);
                    }
                    ControlFlow::Continue(definition.clone())
                }
                _ => ControlFlow::Break(var.clone()),
            })
            .unwrap_or_else(|| {
                unreachable!(
                    "Unbound variable encountered in simplification: {:?}\nin_scope: {:?}",
                    var.id, in_scope
                )
            })
    }

    fn should_inline(
        &self,
        ir: &IR,
        occ: Occurrence,
        in_scope: &InScope,
        ctx: &SimplifierContext,
    ) -> bool {
        match occ {
            Occurrence::Dead | Occurrence::Once => unreachable!(
                "Encountered dead or single-use variable while determining inline viablility. This should have been handled prior.",
            ),
            Occurrence::OnceInFun => ir.is_value() && self.some_benefit(ir, in_scope, ctx),
            Occurrence::Many => {
                let small_enough = ir.size() <= self.inline_size_threshold;
                ir.is_value() && small_enough && self.some_benefit(ir, in_scope, ctx)
            }
        }
    }

    fn some_benefit(&self, ir: &IR, in_scope: &InScope, ctx: &SimplifierContext) -> bool {
        let (params, _) = ir.clone().split_funs();

        let args = ctx
            .iter()
            .rev()
            .map_while(|entry| match entry {
                (ContextEntry::App(arg), _) => Some(Arg::Val(arg)),
                (ContextEntry::TyApp(ty), _) => Some(Arg::Ty(ty)),
                _ => None,
            })
            .collect::<Vec<_>>();

        if args.len() >= params.len() {
            return true;
        }

        args.iter().take(params.len()).any(|arg| match arg {
            Arg::Val(arg) => {
                !arg.is_trivial()
                    || match arg {
                        IR::Var(var) => {
                            matches!(in_scope.get(&var.id), Some(Definition::BoundTo(_, _)))
                        }
                        _ => false,
                    }
            }
            Arg::Ty(_) => false,
        })
    }

    fn rebuild(&mut self, mut ir: IR, in_scope: InScope, mut ctx: SimplifierContext) -> IR {
        while let Some((entry, subst)) = ctx.pop() {
            // dbg!(&ir);
            self.subst = subst;
            match entry {
                ContextEntry::App(arg) => {
                    if let IR::Fun(var, body) = ir {
                        self.saturated_fun_count += 1;
                        return self.simplify(IR::local(var, arg, *body), in_scope, ctx);
                    } else {
                        let arg = self.simplify(arg, in_scope.clone(), vec![]);
                        ir = IR::app(ir, arg);
                    }
                }
                ContextEntry::Field(idx) => ir = IR::field(ir, idx),
                ContextEntry::Tag(ty, idx) => ir = IR::tag(ty, idx, ir),
                ContextEntry::Case(ty, b) => ir = IR::case(ty, ir, b),
                ContextEntry::TyApp(ty) => {
                    ir = if let IR::TyFun(_, body) = ir {
                        self.saturated_ty_fun_count += 1;
                        if let TyApp::Ty(ty) = ty {
                            subst_ty(*body, ty)
                        } else {
                            todo!()
                        }
                    } else {
                        IR::ty_app(ir, ty)
                    }
                }
                ContextEntry::TyFun(kind) => {
                    ir = IR::ty_fun(kind, ir);
                }
                ContextEntry::Local(var, occ, body) => {
                    if ir.is_trivial() {
                        self.locals_inlined += 1;
                        self.subst.insert(var.id, SubstRng::Done(ir));
                        return self.simplify(body, in_scope, ctx);
                    } else {
                        let body = self.simplify(
                            body,
                            in_scope.update(var.id, Definition::BoundTo(ir.clone(), occ)),
                            vec![],
                        );
                        ir = if let Occurrence::Dead = self.occs.lookup_var(&var) {
                            self.locals_inlined += 1;
                            body
                        } else {
                            IR::local(var, ir, body)
                        }
                    }
                }
            }
        }
        ir
    }
}
