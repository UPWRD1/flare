use internment::Intern;
use rustc_hash::FxHashMap;

use crate::{
    passes::{
        backend::lowering::{
            ItemSource,
            ir::{IR, ItemId, Kind, Row, TyApp, Type, TypeVar, Var, VarId},
            lower_types::LowerTypes,
        },
        midend::typing::{self, Evidence, ItemWrapper, Typed},
    },
    resource::rep::{
        Spanned,
        ast::{self, Direction, Expr, NodeId, Untyped},
    },
};

#[derive(Default)]
pub struct VarSupply {
    next: usize,
    cache: FxHashMap<Intern<String>, VarId>,
}

impl VarSupply {
    fn supply_for(&mut self, var: Untyped) -> VarId {
        self.cache
            .entry(var.0.0)
            .or_insert_with(|| {
                let ir_var = self.next;
                self.next += 1;
                VarId(ir_var)
            })
            .to_owned()
    }

    pub fn supply(&mut self) -> VarId {
        let ir_var = self.next;
        self.next += 1;
        VarId(ir_var)
    }
}

#[derive(Default)]
pub struct ItemSupply {
    next: u32,
    cache: FxHashMap<ast::ItemId, ItemId>,
}

impl ItemSupply {
    fn supply_for(&mut self, item: ast::ItemId) -> ItemId {
        self.cache
            .entry(item)
            .or_insert_with(|| {
                let ir_item = self.next;
                self.next += 1;
                ItemId(ir_item)
            })
            .to_owned()
    }

    fn supply(&mut self) -> ItemId {
        let ir_item = self.next;
        self.next += 1;
        ItemId(ir_item)
    }
}

pub struct LowerAst<'source> {
    var_supply: VarSupply,
    types: LowerTypes,
    ev_to_var: FxHashMap<Evidence, Var>,
    pub solved: Vec<(Var, IR)>,
    row_to_ev: &'source FxHashMap<NodeId, Evidence>,
    branch_to_ret_ty: &'source FxHashMap<NodeId, Intern<typing::Type>>,
    item_wrappers: &'source FxHashMap<NodeId, ItemWrapper>,
    item_source: &'source ItemSource,
    item_supply: &'source mut ItemSupply,
}

pub struct LowerSolvedEv<'supply> {
    supply: &'supply mut VarSupply,
    left: Vec<Type>,
    right: Vec<Type>,
    goal: Vec<Type>,

    goal_indices: Vec<RowIndex>,
    left_indices: Vec<usize>,
    right_indices: Vec<usize>,
}

impl LowerSolvedEv<'_> {
    pub fn lower_ev_term(mut self) -> IR {
        IR::tuple([
            self.concat(),
            self.branch(),
            IR::tuple([self.prj_left(), self.inj_left()]),
            IR::tuple([self.prj_right(), self.inj_right()]),
        ])
    }

    fn unwrap_prj(index: usize, len: usize, prod: Var) -> IR {
        Self::unwrap_single(len, prod, |ir| IR::field(ir, index))
    }

    fn unwrap_single(len: usize, var: Var, else_fn: impl FnOnce(IR) -> IR) -> IR {
        if len == 1 {
            IR::Var(var)
        } else {
            else_fn(IR::Var(var))
        }
    }

    fn make_vars<const N: usize>(&mut self, tys: [Type; N]) -> [Var; N] {
        tys.map(|ty| {
            let id = self.supply.supply();
            Var::new(id, ty)
        })
    }

    fn left_prod(&self) -> Type {
        Type::prod(Row::Closed(self.left.clone()))
    }

    fn right_prod(&self) -> Type {
        Type::prod(Row::Closed(self.right.clone()))
    }

    fn goal_prod(&self) -> Type {
        Type::prod(Row::Closed(self.goal.clone()))
    }

    fn left_sum(&self) -> Type {
        Type::sum(Row::Closed(self.left.clone()))
    }

    fn right_sum(&self) -> Type {
        Type::sum(Row::Closed(self.right.clone()))
    }

    fn goal_sum(&self) -> Type {
        Type::sum(Row::Closed(self.goal.clone()))
    }

    fn concat(&mut self) -> IR {
        let vars = self.make_vars([self.left_prod(), self.right_prod()]);
        IR::funs(vars.clone(), {
            let [left, right] = vars;
            let mut elems = self.goal_indices.iter().map(|row_index| match row_index {
                RowIndex::Left(i) => Self::unwrap_prj(*i, self.left.len(), left.clone()),
                RowIndex::Right(i) => Self::unwrap_prj(*i, self.right.len(), right.clone()),
            });
            if self.goal_indices.len() == 1 {
                elems.next().unwrap()
            } else {
                IR::tuple(elems)
            }
        })
    }

    fn branch(&mut self) -> IR {
        let left_sum = self.left_sum().shifted();
        let right_sum = self.right_sum().shifted();
        let goal_sum = self.goal_sum().shifted();
        let ret_ty = Type::Var(TypeVar(0));

        let vars = self.make_vars([
            Type::fun(left_sum.clone(), ret_ty.clone()),
            Type::fun(right_sum.clone(), ret_ty.clone()),
            goal_sum,
        ]);

        IR::ty_fun(
            Kind::Type,
            IR::funs(vars.clone(), {
                let [left_var, right_var, goal_var] = vars;
                let goal_len = self.goal.len();
                let mut branches = self.goal_indices.clone().into_iter().map(|row_index| {
                    let (i, ty, len, var, sum) = match row_index {
                        RowIndex::Left(i) => (
                            i,
                            self.left[i].clone().shifted(),
                            self.left.len(),
                            left_var.clone(),
                            left_sum.clone(),
                        ),
                        RowIndex::Right(i) => (
                            i,
                            self.right[i].clone().shifted(),
                            self.right.len(),
                            right_var.clone(),
                            right_sum.clone(),
                        ),
                    };
                    let [case_var] = self.make_vars([ty]);
                    IR::branch(case_var.clone(), {
                        IR::app(
                            IR::Var(var),
                            Self::unwrap_single(len, case_var, |ir| IR::tag(sum, i, ir)),
                        )
                    })
                });
                if goal_len == 1 {
                    IR::app(branches.next().unwrap().as_fun(), IR::Var(goal_var))
                } else {
                    IR::case(ret_ty, IR::Var(goal_var), branches)
                }
            }),
        )
    }

    // fn left_indices(&self) -> Vec<usize> {
    //     let mut left = self
    //         .goal_indices
    //         .iter()
    //         .enumerate()
    //         .filter_map(|(goal_index, row_index)| match row_index {
    //             RowIndex::Left(left_indx) => Some((*left_indx, goal_index)),
    //             _ => None,
    //         })
    //         .collect::<Vec<_>>();
    //     left.sort_by_key(|(key, _)| *key);
    //     left.into_iter().map(|(_, goal_index)| goal_index).collect()
    // }

    // fn right_indices(&self) -> Vec<usize> {
    //     let mut right = self
    //         .goal_indices
    //         .iter()
    //         .enumerate()
    //         .filter_map(|(goal_index, row_index)| match row_index {
    //             RowIndex::Right(right_index) => Some((*right_index, goal_index)),
    //             _ => None,
    //         })
    //         .collect::<Vec<_>>();
    //     right.sort_by_key(|(key, _)| *key);
    //     right
    //         .into_iter()
    //         .map(|(_, goal_index)| goal_index)
    //         .collect()
    // }

    fn prj_left(&mut self) -> IR {
        let [goal] = self.make_vars([self.goal_prod()]);
        IR::fun(goal.clone(), {
            if self.left.len() == 1 {
                Self::unwrap_prj(self.left_indices[0], self.goal.len(), goal)
            } else {
                IR::tuple(
                    self.left_indices
                        .clone()
                        .into_iter()
                        .map(|i| Self::unwrap_prj(i, self.goal.len(), goal.clone())),
                )
            }
        })
    }

    fn prj_right(&mut self) -> IR {
        let [goal] = self.make_vars([self.goal_prod()]);

        IR::fun(goal.clone(), {
            if self.right.len() == 1 {
                Self::unwrap_prj(self.right_indices[0], self.goal.len(), goal)
            } else {
                IR::tuple(
                    self.right_indices
                        .clone()
                        .into_iter()
                        .map(|i| Self::unwrap_prj(i, self.goal.len(), goal.clone())),
                )
            }
        })
    }

    fn left_enumerated_values(&self) -> impl Iterator<Item = (usize, Type)> + use<> {
        self.left_indices.clone().into_iter().zip(self.left.clone())
    }

    fn right_enumerated_values(&self) -> impl Iterator<Item = (usize, Type)> + use<> {
        self.right_indices
            .clone()
            .into_iter()
            .zip(self.right.clone())
    }

    fn inj_left(&mut self) -> IR {
        let [left_var] = self.make_vars([self.left_sum()]);
        IR::fun(left_var.clone(), {
            let branches = self
                .left_enumerated_values()
                .map(|(i, ty)| {
                    let [branch_var] = self.make_vars([ty]);
                    IR::branch(branch_var.clone(), {
                        Self::unwrap_single(self.goal.len(), branch_var, |ir| {
                            IR::tag(self.goal_sum(), i, ir)
                        })
                    })
                })
                .collect::<Vec<_>>();
            if self.left.len() == 1 {
                IR::app(branches[0].as_fun(), IR::Var(left_var))
            } else {
                IR::case(self.goal_sum(), IR::Var(left_var), branches)
            }
        })
    }
    fn inj_right(&mut self) -> IR {
        let [right_var] = self.make_vars([self.right_sum()]);
        IR::fun(right_var.clone(), {
            let branches = self
                .right_enumerated_values()
                .map(|(i, ty)| {
                    let [branch_var] = self.make_vars([ty]);
                    IR::branch(branch_var.clone(), {
                        Self::unwrap_single(self.goal.len(), branch_var, |ir| {
                            IR::tag(self.goal_sum(), i, ir)
                        })
                    })
                })
                .collect::<Vec<_>>();
            if self.right.len() == 1 {
                IR::app(branches[0].as_fun(), IR::Var(right_var))
            } else {
                IR::case(self.goal_sum(), IR::Var(right_var), branches)
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum RowIndex {
    Left(usize),
    Right(usize),
}
impl<'source> LowerAst<'source> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        var_supply: VarSupply,
        types: LowerTypes,
        ev_to_var: FxHashMap<Evidence, Var>,
        row_to_ev: &'source FxHashMap<NodeId, Evidence>,
        branch_to_ret_ty: &'source FxHashMap<NodeId, Intern<typing::Type>>,
        item_wrappers: &'source FxHashMap<NodeId, ItemWrapper>,
        item_source: &'source ItemSource,
        item_supply: &'source mut ItemSupply,
    ) -> Self {
        Self {
            var_supply,
            types,
            ev_to_var,
            solved: vec![],
            row_to_ev,
            branch_to_ret_ty,
            item_wrappers,
            item_source,
            item_supply,
        }
    }

    fn lookup_ev(&mut self, ev: Evidence) -> Var {
        // If we've seen this evidence before, reuse the variable we already generated.
        // Otherwise, generate a variable for our solved evidence.
        self.ev_to_var
            .entry(ev)
            .or_insert_with_key(|ev| {
                // If we see a vacant entry during lowering it must be solved.
                // All our unsolved evidence appears in the type scheme.
                let Evidence::RowEquation {
                    left: typing::Row::Closed(left),
                    right: typing::Row::Closed(right),
                    goal: typing::Row::Closed(goal),
                } = ev
                else {
                    unreachable!("Unsolved evidence appeared in AST that wasn't in type scheme");
                };
                let param = self.var_supply.supply();

                let mut left_indices = vec![0; left.fields.len()];
                let mut right_indices = vec![0; right.fields.len()];
                let goal_indices = goal
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(goal_indx, field)| {
                        left.fields
                            .binary_search(field)
                            .map(|left_indx| {
                                left_indices[left_indx] = goal_indx;
                                RowIndex::Left(left_indx)
                            })
                            .or_else(|_| {
                                right.fields.binary_search(field).map(|right_indx| {
                                    right_indices[right_indx] = goal_indx;
                                    RowIndex::Right(right_indx)
                                })
                            })
                            .expect("Invalid solved row combination.")
                    })
                    .collect::<Vec<_>>();
                let left_values = self.types.lower_closed_row_ty(*left);
                let right_values = self.types.lower_closed_row_ty(*right);
                let goal_values = self.types.lower_closed_row_ty(*goal);

                let lower_solved_ev = LowerSolvedEv {
                    supply: &mut self.var_supply,
                    left: left_values,
                    right: right_values,
                    goal: goal_values,
                    goal_indices,
                    left_indices,
                    right_indices,
                };

                let term = lower_solved_ev.lower_ev_term();
                let ty = self.types.lower_ev_ty(ev.clone());
                debug_assert_eq!(
                    ty,
                    term.type_of(),
                    "Evidence term did not have the type expected by lower_ev_ty"
                );
                let var = Var::new(param, ty);
                self.solved.push((var.clone(), term));
                var
            })
            .clone()
    }

    pub fn lower_ast(&mut self, ast: Spanned<Intern<Expr<Typed>>>) -> IR {
        let id = ast.1;
        match *ast.0 {
            Expr::Ident(Typed(var, ty)) => IR::Var(Var::new(
                self.var_supply.supply_for(var),
                self.types.lower_ty(*ty),
            )),
            Expr::Number(n) => IR::Num(n),
            Expr::String(n) => IR::Str(n.0),
            Expr::Bool(b) => IR::Bool(b),

            Expr::Particle(p) => IR::Particle(p.0),
            Expr::Lambda(Typed(var, ty), body, _) => {
                let ir_ty = self.types.lower_ty(*ty);
                let ir_var = self.var_supply.supply_for(var);
                let ir_body = self.lower_ast(body);
                IR::fun(Var::new(ir_var, ir_ty), ir_body)
            }
            Expr::Call(fun, arg) => {
                let ir_fun = self.lower_ast(fun);
                let ir_arg = self.lower_ast(arg);
                IR::app(ir_fun, ir_arg)
            }

            Expr::Label(_, body) => self.lower_ast(body),
            Expr::Unlabel(body, _) => self.lower_ast(body),
            Expr::Concat(left, right) => {
                let param = self
                    .row_to_ev
                    .get(&id)
                    .cloned()
                    .map(|ev| self.lookup_ev(ev))
                    .unwrap_or_else(|| unreachable!("Concat AST node lacks evidence"));

                let concat = IR::field(IR::Var(param), 0);
                let left = self.lower_ast(left);
                let right = self.lower_ast(right);
                IR::app(IR::app(concat, left), right)
            }
            Expr::Branch(left, right) => {
                let param = self
                    .row_to_ev
                    .get(&id)
                    .cloned()
                    .map(|ev| self.lookup_ev(ev))
                    .unwrap_or_else(|| unreachable!(" Branch AST node lacks an expected evidence"));

                let ret_ty = self
                    .branch_to_ret_ty
                    .get(&id)
                    .map(|ty| self.types.lower_ty(**ty))
                    .unwrap_or_else(|| unreachable!("Branch AST node lacks expected type"));
                let branch = IR::ty_app(IR::field(IR::Var(param), 1), TyApp::Ty(ret_ty));

                let left = self.lower_ast(left);
                let right = self.lower_ast(right);
                IR::app(IR::app(branch, left), right)
            }

            Expr::Project(direction, body) => {
                let param = self
                    .row_to_ev
                    .get(&id)
                    .cloned()
                    .map(|ev| self.lookup_ev(ev))
                    .expect("Project AST node lacks an expected evidence");

                let term = self.lower_ast(body);
                let direction_field = match direction {
                    Direction::Left => 2,
                    Direction::Right => 3,
                };
                let prj_direction = IR::field(IR::field(IR::Var(param), direction_field), 0);
                IR::app(prj_direction, term)
            }
            Expr::Inject(direction, body) => {
                let param = self
                    .row_to_ev
                    .get(&id)
                    .cloned()
                    .map(|ev| self.lookup_ev(ev))
                    .expect("Inject AST node lacks an expected evidence");

                let term = self.lower_ast(body);
                let direction_field = match direction {
                    Direction::Left => 2,
                    Direction::Right => 3,
                };
                let inj_direction = IR::field(IR::field(IR::Var(param), direction_field), 1);
                IR::app(inj_direction, term)
            }
            Expr::Item(item_id, k) => {
                let ty = self.item_source.lookup_item(item_id);
                match k {
                    ast::Kind::Extern(s) => IR::Extern(s, ty),
                    ast::Kind::Func => {
                        let item_ir = IR::Item(ty, self.item_supply.supply_for(item_id));
                        let wrapper = self
                            .item_wrappers
                            .get(&id)
                            .cloned()
                            .unwrap_or_else(|| unreachable!("Item lacks expected wrapper"));

                        let ty_ir = wrapper.types.into_iter().fold(item_ir, |ir, ty| {
                            IR::ty_app(ir, TyApp::Ty(self.types.lower_ty(*ty)))
                        });
                        let row_ir = wrapper.rows.into_iter().fold(ty_ir, |ir, row| {
                            IR::ty_app(ir, TyApp::Row(self.types.lower_row_ty(row)))
                        });
                        wrapper.evidence.into_iter().fold(row_ir, |ir, ev| {
                            let param = self.lookup_ev(ev);
                            IR::app(ir, IR::Var(param))
                        })
                    }
                    _ => unimplemented!(),
                }
            }
            _ => todo!("{ast:?}"),
        }
    }
}
