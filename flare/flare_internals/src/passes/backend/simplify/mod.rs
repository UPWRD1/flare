use std::ops::ControlFlow;

use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};

use crate::{
    passes::backend::lowering::ir::{Branch, IR, ItemId, Kind, TyApp, Type, Var, VarId},
    resource::rep::ast::BinOp,
};
#[allow(dead_code)]
enum Param {
    Ty(Kind),
    Val(Var),
}
#[allow(dead_code)]
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

            Self::Bin(l, _, r) => l.size() + r.size(),

            Self::Fun(_, body) => 10 + body.size(),
            Self::App(fun, arg) => arg.size() + size_app(fun, 1),
            Self::TyFun(_, ir) | Self::TyApp(ir, _) | Self::Field(ir, _) | Self::Tag(_, _, ir) => {
                ir.size()
            }
            Self::Local(var, defn, body) => {
                defn.size() + body.size() + (if var.ty.is_cheap_alloc() { 0 } else { 10 })
            }
            Self::If(c, t, o) => c.size().max(t.size().max(o.size())),
            Self::Tuple(v) => v.iter().map(Self::size).sum::<usize>(),
            Self::Case(_, s, b) => s.size() + b.iter().map(|x| x.as_fun().size()).sum::<usize>(),
            Self::Item(_, _) | Self::Extern(_, _) => 100,
            // _ => todo!(),
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
            Self::Bin(l, _, r) => l.is_value() && r.is_value(),

            Self::TyFun(_, ir) => ir.is_value(),
            Self::Local(_, defn, body) => defn.is_value() && body.is_value(),
            Self::Tuple(s) => s.iter().all(Self::is_value),
            Self::Var(_)
            | Self::Num(_)
            | Self::Str(_)
            | Self::Unit
            | Self::Bool(_)
            | Self::Particle(_)
            | Self::Fun(_, _)
            | Self::Tag(_, _, _) => true,
            Self::App(_, _) | Self::TyApp(_, _) => false,
            Self::Case(_, _, _) => true,
            _ => todo!("{self:?}"),
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

#[derive(Default, Debug, Clone)]
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

pub fn subst_ty(haystack: IR, payload: TyApp) -> IR {
    match haystack {
        IR::Var(var) => IR::Var(var.map_ty(|ty| ty.subst_app(payload))),
        IR::Num(_)
        | IR::Str(_)
        | IR::Bool(_)
        | IR::Unit
        | IR::Particle(_)
        // c IR::Tuple(_)
        | IR::Tag(_, _, _) | IR::Extern(_, _) => haystack,

        IR::Fun(var, ir) => IR::fun(
            var.map_ty(|ty| ty.subst_app(payload.clone())),
            subst_ty(*ir, payload),
        ),
        IR::App(fun, arg) => IR::app(subst_ty(*fun, payload.clone()), subst_ty(*arg, payload)),
        IR::TyFun(kind, ir) => IR::ty_fun(kind, subst_ty(*ir, payload)),
        IR::TyApp(ir, ty) => IR::ty_app(subst_ty(*ir, payload.clone()), ty.subst_tyapp(payload)),
        IR::Local(var, defn, body) => IR::local(
            var.map_ty(|ty| ty.subst_app(payload.clone())),
            subst_ty(*defn, payload.clone()),
            subst_ty(*body, payload),
        ),
        IR::Field(ir, _u) => {
            // if let IR::Tuple(content) = *ir {
            //     let heads = &content[..u];
            //     let i = content[u].clone();
            //     let tails = &content[u + 1..];
            //     let v = subst_ty(i, payload);
            //     let new_tuple = IR::Tuple([heads, &[v], tails].concat());
            //     // let (i, heads) = items.split_last().unwrap();
            //     // dbg!(new_tuple);
            //     todo!()
            // } else {
                // dbg!(&ir);
                subst_ty(*ir, payload)
            // }
        }

        IR::Tuple(elements) => {
            // dbg!(&elements);
            IR::Tuple(
                elements
                    .into_iter()
                    .map(|elem| subst_ty(elem, payload.clone()))
                    .collect()
            )
        }
        // might be wrong

        IR::Case(t, ir, branches) => {
            IR::Case(t, Box::new(subst_ty(*ir, payload.clone())), branches.into_iter().map(|x| Branch{ param: x.param, body: subst_ty(x.body, payload.clone())}).collect())
        }
        IR::Item(t, id) => {
            IR::Item(t.subst_app(payload), id)
        }
        _ => todo!("{haystack:?}"),
    }
}

struct OccuranceAnalyzer<'i> {
    items: &'i [(IR, Type)],
}
impl<'i> OccuranceAnalyzer<'i> {
    pub fn new(items: &'i [(IR, Type)]) -> Self {
        Self { items }
    }

    fn occurrence_analysis(
        &self,
        ir: &IR,
        seen: &im::HashSet<ItemId, FxBuildHasher>,
    ) -> (FxHashSet<VarId>, Occurrences) {
        match ir {
            IR::Var(var) => {
                let mut free = FxHashSet::default();
                free.insert(var.id);
                (free, Occurrences::with_var_once(var.id))
            }
            IR::Num(_) | IR::Str(_) | IR::Bool(_) | IR::Unit | IR::Particle(_) => {
                (FxHashSet::default(), Occurrences::default())
            }
            IR::Bin(l, _, r) => {
                let (mut free, occs) = self.occurrence_analysis(l, seen);
                let (r_free, r_occs) = self.occurrence_analysis(r, seen);

                free.extend(r_free);
                (free, occs.merge(r_occs))
            }

            IR::Fun(var, ir) => {
                let (mut free, mut occs) = self.occurrence_analysis(ir, seen);
                free.remove(&var.id);
                occs.vars.entry(var.id).or_insert(Occurrence::Dead);
                let occs = occs.in_fun(&free);
                (free, occs)
            }

            IR::App(fun, arg) => {
                let (mut fun_free, fun_occs) = self.occurrence_analysis(fun, seen);
                let (arg_free, arg_occs) = self.occurrence_analysis(arg, seen);
                fun_free.extend(arg_free);
                (fun_free, fun_occs.merge(arg_occs))
            }
            IR::TyFun(_, ir) => self.occurrence_analysis(ir, seen),
            IR::TyApp(ir, _) => self.occurrence_analysis(ir, seen),
            IR::Local(var, defn, body) => {
                let (mut free, mut occs) = self.occurrence_analysis(body, seen);
                let (defn_free, defn_occs) = self.occurrence_analysis(defn, seen);
                free.extend(defn_free);
                free.remove(&var.id);
                occs.vars.entry(var.id).or_insert(Occurrence::Dead);
                (free, defn_occs.merge(occs))
            }
            IR::If(c, t, o) => {
                let (mut free, mut occs) = self.occurrence_analysis(c, seen);

                let (then_free, then_occs) = self.occurrence_analysis(t, seen);

                let (other_free, other_occs) = self.occurrence_analysis(o, seen);

                free.extend(then_free);
                free.extend(other_free);

                occs = occs.merge(then_occs);
                occs = occs.merge(other_occs);

                (free, occs)
            }

            IR::Field(ir, _) => self.occurrence_analysis(ir, seen),
            IR::Tuple(elements) => {
                let mut occs = Occurrences::default();
                let mut free = FxHashSet::default();
                for elem in elements {
                    // dbg!(elem);
                    let (elem_free, elem_occs) = self.occurrence_analysis(elem, seen);
                    free.extend(elem_free);
                    occs = occs.merge(elem_occs)
                }
                (free, occs)
            }
            IR::Case(_, scrutinee, branches) => {
                let (mut scrutinee_free, mut scrutinee_occs) =
                    self.occurrence_analysis(scrutinee, seen);
                for branch in branches {
                    let (mut branch_free, mut branch_occs) =
                        self.occurrence_analysis(&branch.body, seen);
                    branch_free.remove(&branch.param.id);

                    branch_occs
                        .vars
                        .entry(branch.param.id)
                        .or_insert(Occurrence::Dead);
                    scrutinee_free.extend(branch_free);
                    scrutinee_occs = scrutinee_occs.merge(branch_occs)
                }
                (scrutinee_free, scrutinee_occs)
            }

            IR::Tag(_, _, ir) => self.occurrence_analysis(ir, seen),
            IR::Item(_, id) => {
                // dbg!(self.items.len(), id);
                if seen.contains(id) {
                    // We are analyzing a recursive function
                    (Default::default(), Default::default())
                } else {
                    let seen = seen.update(*id);
                    self.occurrence_analysis(&self.items[id.0 as usize].0, &seen)
                }
            }
            IR::Extern(_, _) => (Default::default(), Default::default()),
            // _ => todo!("{ir:?}"),
        }
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

#[derive(Clone, Debug)]
enum ContextEntry {
    App(IR),
    TyApp(TyApp),
    TyFun(Kind),
    Local(Var, Occurrence, IR),
    Field(usize),
    Tag(Type, usize),
    Case(Type, Vec<Branch>),
    Bin(BinOp, IR),
}

// #[derive(Default)]
struct Simplifier<'p> {
    occs: Occurrences,
    subst: Subst,
    saturated_fun_count: usize,
    saturated_ty_fun_count: usize,
    locals_inlined: usize,
    inline_size_threshold: usize,
    items: &'p [(IR, Type)],

    seen_items: FxHashSet<ItemId>,
}

pub fn simplify(the_ir: &[(IR, Type)]) -> Vec<(IR, Type)> {
    the_ir
        // .clone()
        .iter()
        .map(|(ir, t)| {
            let mut ir = ir.clone();
            for _ in 0..2 {
                let occ_a = OccuranceAnalyzer::new(the_ir);
                let (_, occs) =
                    occ_a.occurrence_analysis(&ir, &im::HashSet::with_hasher(FxBuildHasher));
                let mut simplifier = Simplifier::new(occs, the_ir);
                ir = simplifier.simplify(ir, InScope::default(), vec![]);
                if simplifier.did_no_work() {
                    // println!("Simplified after {i} passes");
                    break;
                }
            }
            (ir, t.clone())
        })
        .collect()
}

impl<'p> Simplifier<'p> {
    fn new(occs: Occurrences, prev: &'p [(IR, Type)]) -> Self {
        Self {
            occs,
            items: prev,
            inline_size_threshold: 60, // GHC magic number is 60
            subst: FxHashMap::default(),
            saturated_fun_count: Default::default(),
            saturated_ty_fun_count: Default::default(),
            locals_inlined: Default::default(),

            seen_items: FxHashSet::default(),
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
                IR::Particle(p) => {
                    break self.rebuild(IR::Particle(p), in_scope, ctx);
                }

                IR::Fun(var, body) => {
                    let body =
                        self.simplify(*body, in_scope.update(var.id, Definition::Unknown), vec![]);
                    break self.rebuild(IR::fun(var, body), in_scope, ctx);
                }
                IR::Local(var, defn, body) => self.simplify_local(var, *defn, *body, &mut ctx),
                IR::Var(var) => match self.simplify_var(&var, &in_scope, &ctx) {
                    ControlFlow::Continue(ir) => ir,
                    ControlFlow::Break(var) => {
                        break self.rebuild(IR::Var(var), in_scope, ctx);
                    }
                },

                IR::If(c, t, o) => {
                    let cond = self.simplify(*c, in_scope.clone(), vec![]);
                    let new_ir = if let IR::Bool(b) = cond {
                        if b {
                            self.simplify(*t, in_scope.clone(), vec![])
                        } else {
                            self.simplify(*o, in_scope.clone(), vec![])
                        }
                    } else {
                        let then = self.simplify(*t, in_scope.clone(), vec![]);
                        let other = self.simplify(*o, in_scope.clone(), vec![]);
                        IR::r#if(cond, then, other)
                    };

                    break self.rebuild(new_ir, in_scope, ctx);
                }

                IR::Bin(l, op, r) => {
                    ctx.push((ContextEntry::Bin(op, *r), self.subst.clone()));
                    *l
                }

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
                    if branches.is_empty() {
                        break self.rebuild(*scrutinee, in_scope, ctx);
                    }
                    let branches: Vec<Branch> = branches
                        .into_iter()
                        .map(|b| self.simplify_branch(b, &in_scope, vec![]))
                        .collect();

                    ctx.push((ContextEntry::Case(ty, branches), self.subst.clone()));
                    *scrutinee
                }

                IR::Tag(ty, idx, ir) => {
                    ctx.push((ContextEntry::Tag(ty, idx), self.subst.clone()));
                    *ir
                }
                IR::Extern(_, _) => {
                    break self.rebuild(ir, in_scope, ctx);
                }
                IR::Item(_, itemid) => {
                    // dbg!(self.items.len());
                    // break self.rebuild(ir, in_scope, ctx);
                    match self.item_inline(itemid, &in_scope, &ctx) {
                        ControlFlow::Continue(c) => c,
                        ControlFlow::Break(_) => {
                            break self.rebuild(ir, in_scope, ctx);
                        }
                    }
                }
            }
        }
    }

    fn simplify_branch(&mut self, b: Branch, in_scope: &InScope, ctx: SimplifierContext) -> Branch {
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
        var: &Var,
        in_scope: &InScope,
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
        var: &Var,
        in_scope: &InScope,
        ctx: &SimplifierContext,
    ) -> ControlFlow<Var, IR> {
        in_scope
            .get(&var.id)
            .map(|bind| match bind {
                Definition::BoundTo(definition, occ)
                    if self.should_inline(definition, *occ, in_scope, ctx) =>
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

    fn item_inline(
        &mut self,
        itemid: ItemId,
        in_scope: &InScope,
        ctx: &SimplifierContext,
    ) -> ControlFlow<ItemId, IR> {
        if self.seen_items.contains(&itemid) {
            ControlFlow::Break(itemid)
        } else {
            self.seen_items.insert(itemid);
            self.items
                .get(itemid.0 as usize)
                .map(|(definition, _)| {
                    if definition.size() < self.inline_size_threshold * 2
                        && self.some_benefit(definition, in_scope, ctx)
                    {
                        self.subst = Subst::default();
                        ControlFlow::Continue(definition.clone())
                    } else {
                        ControlFlow::Break(itemid)
                    }
                })
                .unwrap_or_else(|| {
                    unreachable!(
                        "Unknown item encountered in simplification: {:?}\nin_scope: {:?}",
                        itemid, in_scope
                    )
                })
        }
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
                // dbg!(&ir);
                // dbg!(ir.size());
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
            self.subst = subst;
            match entry {
                ContextEntry::App(arg) => {
                    if let IR::Fun(var, body) = ir {
                        self.saturated_fun_count += 1;
                        return self.simplify(IR::local(var, arg, *body), in_scope, ctx);
                    } else if let IR::Item(_, ref id) = ir {
                        if self.seen_items.contains(id) {
                            ir = IR::app(ir, arg);
                        } else {
                            panic!()
                        }
                    } else {
                        // dbg!(&ir, &arg);
                        let arg = self.simplify(arg, in_scope.clone(), vec![]);
                        ir = IR::app(ir, arg);
                    }
                }
                ContextEntry::Field(idx) => {
                    if let IR::Tuple(irs) = ir {
                        let field = irs[idx].clone();
                        return self.simplify(field, in_scope, ctx);
                    } else {
                        // dbg!(&ir);
                        let obj = self.simplify(ir, in_scope.clone(), vec![]);
                        ir = IR::field(obj, idx);
                    }
                }
                ContextEntry::Tag(ty, idx) => ir = IR::tag(ty, idx, ir),
                ContextEntry::Case(ty, b) => ir = IR::case(ty, ir, b),
                ContextEntry::TyApp(tyapp) => {
                    ir = if let IR::TyFun(_, body) = ir {
                        // dbg!(&body);
                        self.saturated_ty_fun_count += 1;
                        subst_ty(*body, tyapp)
                    } else {
                        IR::ty_app(ir, tyapp)
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
                ContextEntry::Bin(op, r) => {
                    ir = match (ir, r) {
                        (IR::Num(l), IR::Num(r)) => match op {
                            BinOp::Eq => IR::Bool(l == r),
                            BinOp::Neq => IR::Bool(l != r),
                            BinOp::Gt => IR::Bool(l > r),
                            BinOp::Lt => IR::Bool(l < r),
                            BinOp::Gte => IR::Bool(l >= r),
                            BinOp::Lte => IR::Bool(l <= r),
                            BinOp::Add => IR::Num(l + r),
                            BinOp::Sub => IR::Num(l - r),
                            BinOp::Mul => IR::Num(l * r),
                            BinOp::Div => IR::Num(l / r),
                            BinOp::And | BinOp::Or => panic!("Invalid operator for num"),
                        },
                        (IR::Bool(l), IR::Bool(r)) => match op {
                            BinOp::Eq => IR::Bool(l == r),
                            BinOp::Neq => IR::Bool(l != r),
                            BinOp::And => IR::Bool(l && r),
                            BinOp::Or => IR::Bool(l || r),
                            _ => panic!("Invalid operator for bool"),
                        },
                        (l, r) => {
                            let r = self.simplify(r, in_scope.clone(), vec![]);
                            IR::bin(l, op, r)
                        }
                    }
                }
            }
        }
        ir
    }
}
