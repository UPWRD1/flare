use std::collections::BTreeSet;

use internment::Intern;

use crate::{
    passes::midend::typing::{
        ClosedRow, Evidence, ItemWrapper, Row, Solver, Type, Typed,
        rows::{RowCombination, RowVar},
        types::TypeVar,
    },
    resource::rep::{
        Spanned,
        ast::{Expr, NodeId},
    },
};

#[derive(Debug)]
pub struct SubstOut<T> {
    pub unbound_tys: BTreeSet<TypeVar>,
    pub unbound_rows: BTreeSet<RowVar>,
    pub value: T,
}

impl<T> SubstOut<T> {
    pub(super) fn new(value: T) -> Self {
        Self {
            unbound_tys: BTreeSet::default(),
            unbound_rows: BTreeSet::default(),
            value,
        }
    }

    fn with_unbound_ty(mut self, ty_var: TypeVar) -> Self {
        self.unbound_tys.insert(ty_var);
        self
    }

    fn with_unbound_row(mut self, row_var: RowVar) -> Self {
        self.unbound_rows.insert(row_var);
        self
    }

    pub(super) fn merge<U, O>(
        mut self,
        other: SubstOut<U>,
        merge_values: impl FnOnce(T, U) -> O,
    ) -> SubstOut<O> {
        self.unbound_tys.extend(other.unbound_tys);
        self.unbound_rows.extend(other.unbound_rows);
        SubstOut {
            unbound_rows: self.unbound_rows,
            unbound_tys: self.unbound_tys,
            value: merge_values(self.value, other.value),
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> SubstOut<U> {
        SubstOut {
            value: f(self.value),
            unbound_tys: self.unbound_tys,
            unbound_rows: self.unbound_rows,
        }
    }
}

impl Solver<'_> {
    fn substitute_closedrow(&mut self, row: ClosedRow) -> SubstOut<ClosedRow> {
        let mut row_out = SubstOut::new(());
        let values = row
            .values
            .iter()
            .map(|ty| {
                let out = self.substitute_ty(*ty);
                row_out.unbound_rows.extend(out.unbound_rows);
                row_out.unbound_tys.extend(out.unbound_tys);
                out.value
            })
            .collect::<Vec<_>>()
            .leak();
        row_out.map(|()| ClosedRow {
            fields: row.fields,
            values,
        })
    }

    fn substitute_row(&mut self, row: Spanned<Intern<Row>>) -> SubstOut<Spanned<Intern<Row>>> {
        match *row.0 {
            Row::Open(v) => SubstOut::new(row.convert(Row::Open(v))),
            Row::Unifier(var) => {
                let root = self.tables.row_unification_table.find(var);

                match self.tables.row_unification_table.probe_value(root) {
                    Some(Row::Unifier(_)) => unreachable!(
                        "Unexpected open row found as value of row unification table. This variable should've been `unify_var_var()`, not `unify_var_value()`"
                    ),
                    Some(Row::Open(v)) => SubstOut::new(row.convert(Row::Open(v))),
                    Some(Row::Closed(crow)) => self
                        .substitute_closedrow(crow)
                        .map(Row::Closed)
                        .map(|cr| row.convert(cr)),
                    None => {
                        let rowvar = self.rowvar_for_unifier(root);
                        SubstOut::new(row.convert(Row::Open(rowvar))).with_unbound_row(rowvar)
                    }
                }
            }
            Row::Closed(crow) => self
                .substitute_closedrow(crow)
                .map(Row::Closed)
                .map(|cr| row.convert(cr)),
        }
    }

    pub fn substitute_ty(&mut self, ty: Spanned<Intern<Type>>) -> SubstOut<Spanned<Intern<Type>>> {
        match *ty.0 {
            Type::Num => SubstOut::new(ty.convert(Type::Num)),
            Type::String => SubstOut::new(ty.convert(Type::String)),
            Type::Bool => SubstOut::new(ty.convert(Type::Bool)),
            Type::Unit => SubstOut::new(ty.convert(Type::Unit)),

            Type::Particle(p) => SubstOut::new(ty.convert(Type::Particle(p))),
            Type::Var(v) => SubstOut::new(ty.convert(Type::Var(v))),
            Type::Unifier(v) => {
                let root = self.tables.unification_table.find(v);
                match self.tables.unification_table.probe_value(root) {
                    Some(t) => self.substitute_ty(t),
                    None => {
                        let ty_var = self.tyvar_for_unifier(root);
                        SubstOut::new(ty.convert(Type::Var(ty_var))).with_unbound_ty(ty_var)
                    }
                }
            }
            Type::Func(arg, ret) => {
                let arg_out = self.substitute_ty(arg);
                let ret_out = self.substitute_ty(ret);
                arg_out.merge(ret_out, |arg, ret| ty.convert(Type::Func(arg, ret)))
            }
            Type::Label(field, value) => self
                .substitute_ty(value)
                .map(|t| ty.convert(Type::Label(field, t))),
            Type::Prod(row) => self
                .substitute_row(row)
                .map(Type::Prod)
                .map(|t| ty.convert(t)),
            Type::Sum(row) => self
                .substitute_row(row)
                .map(Type::Sum)
                .map(|t| ty.convert(t)),

            _ => todo!("{ty:?}"),
        }
    }

    pub fn substitute_ast(
        &mut self,
        unsub_ast: Spanned<Intern<Expr<Typed>>>,
    ) -> SubstOut<Spanned<Intern<Expr<Typed>>>> {
        let id = unsub_ast.1;
        match *unsub_ast.0 {
            Expr::Ident(v) => self
                .substitute_ty(v.1)
                .map(|ty| unsub_ast.convert(Expr::Ident(Typed(v.0, ty)))),

            Expr::Number(i) => SubstOut::new(unsub_ast.convert(Expr::Number(i))),
            Expr::String(s) => SubstOut::new(unsub_ast.convert(Expr::String(s))),
            Expr::Bool(b) => SubstOut::new(unsub_ast.convert(Expr::Bool(b))),
            Expr::Unit => SubstOut::new(unsub_ast.convert(Expr::Unit)),
            Expr::Particle(p) => SubstOut::new(unsub_ast.convert(Expr::Particle(p))),

            Expr::Add(l, r) => self
                .substitute_ast(l)
                .merge(self.substitute_ast(r), |l, r| {
                    unsub_ast.convert(Expr::Add(l, r))
                }),
            Expr::Sub(l, r) => self
                .substitute_ast(l)
                .merge(self.substitute_ast(r), |l, r| {
                    unsub_ast.convert(Expr::Sub(l, r))
                }),
            Expr::Mul(l, r) => self
                .substitute_ast(l)
                .merge(self.substitute_ast(r), |l, r| {
                    unsub_ast.convert(Expr::Mul(l, r))
                }),
            Expr::Div(l, r) => self
                .substitute_ast(l)
                .merge(self.substitute_ast(r), |l, r| {
                    unsub_ast.convert(Expr::Mul(l, r))
                }),
            Expr::Comparison(l, op, r) => {
                SubstOut::new(unsub_ast.convert(Expr::Comparison(l, op, r)))
            }

            Expr::Hole(v) => self
                .substitute_ty(v.1)
                .map(|ty| unsub_ast.convert(Expr::Hole(Typed(v.0, ty)))),
            Expr::Lambda(arg, body, is_anon) => self
                .substitute_ty(arg.1)
                .map(|ty| Typed(arg.0, ty))
                .merge(self.substitute_ast(body), |arg, body| {
                    unsub_ast.convert(Expr::Lambda(arg, body, is_anon))
                }),
            Expr::Call(fun, arg) => self
                .substitute_ast(fun)
                .merge(self.substitute_ast(arg), |fun, arg| {
                    unsub_ast.convert(Expr::Call(fun, arg))
                }),

            Expr::If(cond, then, otherwise) => self
                .substitute_ast(then)
                .merge(self.substitute_ast(otherwise), |t, o| (t, o))
                .merge(self.substitute_ast(cond), |(t, o), c| (t, o, c))
                .map(|(t, o, c)| unsub_ast.convert(Expr::If(c, t, o))),
            Expr::Let(var, def, body) => self
                .substitute_ty(var.1)
                .map(|ty| Typed(var.0, ty))
                .merge(self.substitute_ast(def), |v, d| (v, d))
                .merge(self.substitute_ast(body), |(v, def), body| (v, def, body))
                .map(|(v, def, body)| unsub_ast.convert(Expr::Let(v, def, body))),

            // Label constructor and destructor
            Expr::Label(label, ast) => self
                .substitute_ast(ast)
                .map(|nast| unsub_ast.convert(Expr::Label(label, nast))),
            Expr::Unlabel(ast, label) => self
                .substitute_ast(ast)
                .map(|nast| unsub_ast.convert(Expr::Label(label, nast))),
            // Products constructor and destructor
            Expr::Concat(left, right) => self
                .substitute_ast(left)
                .merge(self.substitute_ast(right), |left, right| {
                    unsub_ast.convert(Expr::Concat(left, right))
                }),
            Expr::Project(dir, ast) => self
                .substitute_ast(ast)
                .map(|nast| unsub_ast.convert(Expr::Project(dir, nast))),
            // Sums constructor and destructor
            Expr::Branch(left, right) => self
                .substitute_ast(left)
                .merge(self.substitute_ast(right), |left, right| {
                    unsub_ast.convert(Expr::Branch(left, right))
                }),
            Expr::Inject(dir, ast) => self
                .substitute_ast(ast)
                .map(|nast| unsub_ast.convert(Expr::Inject(dir, nast))),
            Expr::Item(id, item) => SubstOut::new(unsub_ast.convert(Expr::Item(id, item))),
            _ => todo!("{unsub_ast:?}"),
        }
        // dbg!(res)
    }

    pub fn substitute_wrapper(&mut self, wrapper: ItemWrapper) -> SubstOut<ItemWrapper> {
        fn transpose<T>(vec: Vec<SubstOut<T>>) -> SubstOut<Vec<T>> {
            let mut subst = SubstOut::new(vec![]);
            for ele in vec {
                subst.unbound_tys.extend(ele.unbound_tys);
                subst.unbound_rows.extend(ele.unbound_rows);
                subst.value.push(ele.value);
            }
            subst
        }

        transpose(
            wrapper
                .types
                .into_iter()
                .map(|ty| self.substitute_ty(ty))
                .collect(),
        )
        .merge(
            transpose(
                wrapper
                    .rows
                    .into_iter()
                    .map(|row| self.substitute_row(row))
                    .collect(),
            ),
            |t, r| (t, r),
        )
        .merge(
            transpose(
                wrapper
                    .evidence
                    .into_iter()
                    .map(|ev| self.substitute_evidence(&ev))
                    .collect(),
            ),
            |(types, rows), evidence| ItemWrapper {
                types,
                rows,
                evidence,
            },
        )
    }

    pub fn substitute_evidence(&mut self, ev: &Evidence) -> SubstOut<Evidence> {
        match ev {
            Evidence::RowEquation { left, right, goal } => self
                .substitute_row(*left)
                .merge(self.substitute_row(*right), |l, r| (l, r))
                .merge(self.substitute_row(*goal), |(left, right), goal| {
                    Evidence::RowEquation { left, right, goal }
                }),
        }
    }

    pub fn substitute_row_comb(&mut self, comb: RowCombination) -> SubstOut<Evidence> {
        // dbg!(comb);
        self.substitute_row(comb.left)
            .merge(self.substitute_row(comb.right), |l, r| (l, r))
            .merge(self.substitute_row(comb.goal), |(left, right), goal| {
                Evidence::RowEquation { left, right, goal }
            })
    }
}
