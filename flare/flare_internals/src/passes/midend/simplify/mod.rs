use std::ops::ControlFlow;

use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};

use crate::resource::rep::{frontend::ast::BinOp, midend::{ir::{Branch, IR,  Var, VarId}, irtype::{IRType, Kind, Row, TyApp}}};
#[allow(dead_code)]
enum Arg<'a> {
    Val(&'a IR),
    Ty(&'a TyApp),
}

#[derive(Debug, Clone, Copy)]
enum Occurrence {
    /// Never occurs
    Dead,
    /// Appears once
    Once,
    /// Appears once inside a function
    OnceInFun,
    /// Appears once inside a branch
    OnceInBranch,
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
                            Occurrence::Once if free.contains(&id) =>  Occurrence::OnceInFun,
                            occ => occ,
                        },
                    )
                })
                .collect(),
        }
    }

    
fn in_branch(self, free: &FxHashSet<VarId>) -> Self {
        Self {
            vars: self
                .vars
                .into_iter()
                .map(|(id, occ)| {
                    (
                        id,
                        match occ {
                            Occurrence::Once if free.contains(&id) =>  Occurrence::OnceInBranch,
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
//                         (Occurrence::Many | Occurrence::Once | Occurrence::OnceInFun  |  Occurrence::OnceInBranch, _) |
// (_, Occurrence::Many) | (Occurrence::Once, Occurrence::Once) => Occurrence::Many,
(_, _) => Occurrence::Many,
                    };
                })
                .or_insert(occ);
        }
        self
    }
}

pub fn subst_ty(haystack: IR, payload: IRType) -> IR {
    match haystack {
        IR::Var(var) => IR::Var(var.map_ty(|ty| ty.subst_ty(payload))),
        IR::Num(_)
        | IR::Str(_)
        | IR::Bool(_)
        | IR::Unit
        | IR::Particle(_)
        // c IR::Tuple(_)
         => haystack,
        IR::Fun(var, ir) => IR::fun(
            var.map_ty(|ty| ty.subst_ty(payload.clone())),
            subst_ty(*ir, payload),
        ),
        IR::App(fun, arg) => IR::app(subst_ty(*fun, payload.clone()), subst_ty(*arg, payload)),

IR::TyFun(kind, ir) => IR::ty_fun(kind, subst_ty(*ir, payload)),
    IR::TyApp(ir, ty) => IR::ty_app(
      subst_ty(*ir, payload.clone()),
      match ty {
        TyApp::Ty(ty) => TyApp::Ty(ty.subst_ty(payload)),
        TyApp::Row(row) => TyApp::Row(row.subst_ty(payload)),
      },
    ),
                IR::Local(var, defn, body) => IR::local(
            var.map_ty(|ty| ty.subst_ty(payload.clone())),
            subst_ty(*defn, payload.clone()),
            subst_ty(*body, payload),
        ),
        IR::Field(ir, u) => {
                           IR::field(subst_ty(*ir, payload), u)
    
        }
        IR::Tag(t, u, ir) => IR::tag(t.subst_ty(payload.clone()), u, subst_ty(*ir, payload)),
        IR::Tuple(elements) => {
            // dbg!(&elements);
            IR::tuple(
                elements
                    .into_iter()
                    .map(|elem| subst_ty(elem, payload.clone()))
                                )
        }
        IR::Case(t, ir, branches) => {
            // dbg!(&t, &payload);
            IR::case(
                // t,
                t.subst_ty(payload.clone()),
                // *ir,
                subst_ty(*ir, payload.clone()),
                branches.into_iter().map(|x|
                    Branch{
                        param: x.param.map_ty(|ty| ty.subst_ty(payload.clone())),
                        body: subst_ty(x.body, payload.clone())
                    }
                )
            )

    
                    }
        IR::Item(t, id) => {
            IR::Item(t.subst_ty(payload), id)
        },
        IR::Extern(n, t) => {
            IR::Extern(n, t.subst_ty(payload))
        }
       
        IR::If(ir, t, o) => {
            IR::r#if(subst_ty(*ir, payload.clone()), subst_ty(*t, payload.clone()), subst_ty(*o, payload))
        },
        IR::Bin(ir, bin_op, ir1) => todo!(),
    }
}


pub fn subst_row(haystack: IR, payload: Row) -> IR {
  match haystack {
    IR::Var(var) => IR::Var(var.map_ty(|ty| ty.subst_row(payload))),
    
 IR::Num(_)
        | IR::Str(_)
        | IR::Bool(_)
        | IR::Unit
        | IR::Particle(_)
        // c IR::Tuple(_)
        | IR::Extern(_, _) => haystack,
    IR::Fun(var, ir) => IR::fun(
      var.map_ty(|ty| ty.subst_row(payload.clone())),
      subst_row(*ir, payload),
    ),
    IR::App(fun, arg) => IR::app(subst_row(*fun, payload.clone()), subst_row(*arg, payload)),
    IR::TyFun(kind, ir) => IR::ty_fun(kind, subst_row(*ir, payload)),
    IR::TyApp(ir, ty) => IR::ty_app(
      subst_row(*ir, payload.clone()),
      match ty {
        TyApp::Ty(ty) => TyApp::Ty(ty.subst_row(payload)),
        TyApp::Row(row) => TyApp::Row(row.subst_row(payload)),
      },
    ),
    IR::Tuple(elems) => IR::tuple(
      elems
        .into_iter()
        .map(|elem| subst_row(elem, payload.clone())),
    ),
    IR::Field(ir, indx) => IR::field(subst_row(*ir, payload), indx),
    IR::Tag(ty, tag, ir) => IR::tag(ty.subst_row(payload.clone()), tag, subst_row(*ir, payload)),
    IR::Case(ty, ir, branches) => IR::case(
      ty.subst_row(payload.clone()),
      subst_row(*ir, payload.clone()),
      branches.into_iter().map(|branch| Branch {
        param: branch.param.map_ty(|ty| ty.subst_row(payload.clone())),
        body: subst_row(branch.body, payload.clone()),
      }),
    ),
    IR::Local(var, defn, body) => IR::local(
      var.map_ty(|ty| ty.subst_row(payload.clone())),
      subst_row(*defn, payload.clone()),
      subst_row(*body, payload),
    ),

IR::Item(t, id) => {
            IR::Item(t.subst_row(payload), id)
        }

         IR::If(ir, t, o) => {
            IR::r#if(subst_row(*ir, payload.clone()), subst_row(*t, payload.clone()), subst_row(*o, payload))
        },
        IR::Bin(ir, bin_op, ir1) => todo!(),
  }
}
struct OccuranceAnalyzer {
    // items: &'i [IR],
    in_branch: bool,
}
impl OccuranceAnalyzer {
    pub fn new() -> Self {
        Self { in_branch: false }
    }

    fn occurrence_analysis(
        &mut self,
        ir: &IR,
        // seen: &im::HashSet<ItemId, FxBuildHasher>,
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
            // IR::Comment(_, r) => self.occurrence_analysis(r, seen),
            IR::Bin(l, _, r) => {
                let (mut free, occs) = self.occurrence_analysis(l, );
                let (r_free, r_occs) = self.occurrence_analysis(r, );

                free.extend(r_free);
                (free, occs.merge(r_occs))
            }

            IR::Fun(var, ir) => {
                let (mut free, mut occs) = self.occurrence_analysis(ir);
                free.remove(&var.id);
                occs.vars.entry(var.id).or_insert(Occurrence::Dead);
                let occs = occs.in_fun(&free);
                (free, occs)
            }

            IR::App(fun, arg) => {
                let (mut fun_free, fun_occs) = self.occurrence_analysis(fun);
                let (arg_free, arg_occs) = self.occurrence_analysis(arg);
                fun_free.extend(arg_free);
                (fun_free, fun_occs.merge(arg_occs))
            }
            IR::TyFun(_, ir) | IR::TyApp(ir, _) | IR::Field(ir, _) | IR::Tag(_, _, ir) => self.occurrence_analysis(ir),
            IR::Local(var, defn, body) => {
                let (mut free, mut occs) = self.occurrence_analysis(body);
                let (defn_free, defn_occs) = self.occurrence_analysis(defn);
                free.extend(defn_free);
                free.remove(&var.id);
                occs.vars.entry(var.id).or_insert(Occurrence::Dead);
                (free, defn_occs.merge(occs))
            }
            IR::If(c, t, o) => {
                let (mut free, mut occs) = self.occurrence_analysis(c);

                let (then_free, then_occs) = self.occurrence_analysis(t);

                let (other_free, other_occs) = self.occurrence_analysis(o);

                free.extend(then_free);
                free.extend(other_free);

                occs = occs.merge(then_occs);
                occs = occs.merge(other_occs);

                (free, occs)
            }

            IR::Tuple(elements) => {
                let mut occs = Occurrences::default();
                let mut free = FxHashSet::default();
                for elem in elements {
                    // dbg!(elem);
                    let (elem_free, elem_occs) = self.occurrence_analysis(elem);
                    free.extend(elem_free);
                    occs = occs.merge(elem_occs);
                }
                (free, occs)
            }
            IR::Case(_, scrutinee, branches) => {
                let (mut scrutinee_free, mut scrutinee_occs) =
                    self.occurrence_analysis(scrutinee);
                self.in_branch = true;
                for branch in branches {
                    let (mut branch_free, mut branch_occs) =
                        self.occurrence_analysis(&branch.body);
                    branch_free.remove(&branch.param.id);

                    branch_occs
                        .vars
                        .entry(branch.param.id)
                        .or_insert(Occurrence::Dead);

                    branch_occs = branch_occs.in_fun(&branch_free);
                    scrutinee_free.extend(branch_free);
                    scrutinee_occs = scrutinee_occs.merge(branch_occs);
                }
                self.in_branch = false;
                (scrutinee_free, scrutinee_occs)
            }

            IR::Item(_, id) => {
                // dbg!(self.items.len(), id);
                // if seen.contains(id) {
                    // We are analyzing a recursive function
                    (Default::default(), Default::default())
                // } else {
                //     let seen = seen.update(*id);
                //     self.occurrence_analysis(&self.items[id.0 as usize], &seen)
                // }
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
    Tag(IRType, usize),
    Case(IRType, Vec<Branch>),
    Bin(BinOp, IR),
}

#[derive(Default)]
struct Simplifier {
    occs: Occurrences,
    subst: Subst,
    saturated_fun_count: usize,
    saturated_ty_fun_count: usize,
    locals_inlined: usize,
    tuples_inlined: usize,
    inline_size_threshold: usize,
    // items: &'p [IR],

    // seen_items: FxHashSet<ItemId>,
}

pub fn simplify(the_ir: Vec<IR>) -> Vec<IR> {
let ref_ir = the_ir.clone();
let mut occ_a = OccuranceAnalyzer::new();
let mut simplifier = Simplifier::new();
    the_ir
                .into_iter()
        .map(|ir| {
            let mut ir = ir;
            for _ in 0..2 {
            let (_, occs) =
                occ_a.occurrence_analysis(&ir);
                simplifier.with_occs(occs);
                ir = simplifier.simplify(ir, InScope::default(), vec![]);
                if simplifier.did_no_work() {
                    // println!("Simplified after {i} passes");
                    break;
                }
            }
            ir
        })
        .collect()
}

impl Simplifier {
    fn new() -> Self {
        Self {
            // occs:,
            // items ,
            inline_size_threshold: 60, // GHC magic number is 60
            subst: FxHashMap::default(),
            ..Default::default()        }
    }
    fn with_occs(&mut self, occs: Occurrences) {
        self.occs = occs
    }
        
    fn did_no_work(&self) -> bool {
        self.saturated_fun_count == 0
            && self.saturated_ty_fun_count == 0
            && self.locals_inlined == 0
        && self.tuples_inlined == 0
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
                                // IR::Comment(_, r) => *r,

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
                        .map(|elem| self.simplify(elem, in_scope.clone(), Vec::new()))
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
                        .map(|b| self.simplify_branch(b, &in_scope))
                        .collect();

                    ctx.push((ContextEntry::Case(ty, branches), self.subst.clone()));
                    *scrutinee
                }
                IR::Tag(ty, idx, body) => {
                    ctx.push((ContextEntry::Tag(ty, idx), self.subst.clone()));
                    *body
                }
                IR::Extern(_, _) => {
                    break self.rebuild(ir, in_scope, ctx);
                }
                IR::Item(ref t, itemid) => {
                    break self.rebuild(ir, in_scope, ctx);
                }                           
            }
        }
    }

    fn simplify_branch(&mut self, b: Branch, in_scope: &InScope) -> Branch {
        Branch {
            body: self.simplify(
                b.body,
                in_scope.update(b.param.id, Definition::Unknown),
                Vec::new(),
            ),
            ..b
        }
    }

    fn simplify_local(&mut self, var: Var, defn: IR, body: IR, ctx: &mut SimplifierContext) -> IR {
        if let IR::Var(ref v) = defn && *v == var {
            return body
            
        }
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
            .map_or_else(
|| {
                unreachable!(
                    "Unbound variable encountered in simplification: {:?}\nin_scope: {:?}",
                    var.id, in_scope
                )
            }     ,           |bind| match bind {
                Definition::BoundTo(definition, occ)
                    if self.should_inline(definition, *occ, in_scope, ctx) =>
                {
                    self.subst = Subst::default();
                    if matches!(occ, Occurrence::OnceInFun) {
                        self.occs.mark_dead(var.id);
                    }
                    ControlFlow::Continue(definition.clone())
                }
                _ => ControlFlow::Break(var.clone()),
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
            Occurrence::OnceInBranch |
            Occurrence::Many => {
                // dbg!(&ir);
                // dbg!(ir.size());
                let small_enough = ir.size() <= self.inline_size_threshold   ;
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
                        let arg = self.simplify(arg, in_scope.clone(), Vec::new());
                            ir = IR::app(ir, arg);
                                                                    } else {
                        // dbg!(&ir, &arg);
                        let arg = self.simplify(arg, in_scope.clone(), Vec::new());
                        ir = IR::app(ir, arg);
                    }
                }
                ContextEntry::Field(idx) => {
                    if let IR::Tuple(ref irs) = ir {
                        self.tuples_inlined += 1;

                        let field = irs[idx].clone();
                        return self.simplify(field, in_scope, ctx);
                    } else{
                        let obj = self.simplify(ir, in_scope.clone(), Vec::new());
                        ir = IR::field(obj, idx);
                     }
                }
                ContextEntry::Tag(ty, idx) => ir = IR::tag(ty, idx, ir),
                ContextEntry::Case(ty, b) => {
                    if let IR::Tag(ty, idx, body) = ir {
                        let correct_branch = b[idx].clone();
                        let branch_as_fn = correct_branch.as_fun();
                        ir = IR::app(branch_as_fn, *body);
                    } else {
                        ir = IR::case(ty, ir, b)
                    }
                },
                ContextEntry::TyApp(ty_app) => {
                    ir = if let IR::TyFun(_, body) = ir {
                        // dbg!(&body);
                            self.saturated_ty_fun_count += 1;
                            match ty_app {
                                TyApp::Ty(ty) => subst_ty(*body, ty),
                                TyApp::Row(row) => subst_row(*body, row),
                            }                                               
                        } else {
                            IR::ty_app(ir, ty_app)
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
                        ir = if matches!(self.occs.lookup_var(&var), Occurrence::Dead) {
                            self.locals_inlined += 1;
                            body
                        } else {
                            IR::local(var, ir, body)
                        }
                    }
                }
                ContextEntry::Bin(op, r) => {
                    ir = self.rebuild_binop(ir, op, r, in_scope.clone())
                }
            }
        }
        ir
    }

    fn rebuild_binop(&mut self, l: IR, op: BinOp, r: IR, in_scope: InScope) -> IR {
        match (l, r) {
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
                        // _ => panic!("Invalid binop: {} {} {}", l, op, r)
                        (l, r) => {
                             let r = self.simplify(r, in_scope, Vec::new());
                             IR::bin(l, op, r)
                        }
                    }    }
}
