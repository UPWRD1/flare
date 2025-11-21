use std::collections::BTreeSet;

use internment::Intern;

use crate::{passes::midend::typing::{ClosedRow, Evidence, Row, Solver, TyUniVar, Type, Typed, rows::{RowCombination, RowUniVar}}, resource::rep::{Spanned, ast::Expr}, };
pub struct SubstOut<T> {
  pub unbound_tys: BTreeSet<TyUniVar>,
  pub unbound_rows: BTreeSet<RowUniVar>,
  pub value: T,
}

impl<T> SubstOut<T> {
  pub(super) fn new(value: T) -> Self {
    Self {
      unbound_tys: Default::default(),
      unbound_rows: Default::default(),
      value,
    }
  }

  fn insert_unbound_ty(&mut self, ty_var: TyUniVar) {
    self.unbound_tys.insert(ty_var);
  }
  fn with_unbound_ty(mut self, ty_var: TyUniVar) -> Self {
    self.insert_unbound_ty(ty_var);
    self
  }

  fn with_unbound_row(mut self, row_var: RowUniVar) -> Self {
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

  fn map<U>(self, f: impl FnOnce(T) -> U) -> SubstOut<U> {
    SubstOut {
      value: f(self.value),
      unbound_tys: self.unbound_tys,
      unbound_rows: self.unbound_rows,
    }
  }
}

impl<'env> Solver<'env> {
  fn substitute_closedrow(&mut self, row: ClosedRow) -> SubstOut<ClosedRow> {
    let mut row_out = SubstOut::new(());
    let values = row
      .values
      .into_iter()
      .map(|ty| {
        let out = self.substitute_ty(*ty);
        row_out.unbound_rows.extend(out.unbound_rows);
        row_out.unbound_tys.extend(out.unbound_tys);
        out.value
      })
      .collect::<Vec<_>>().leak();
    row_out.map(|_| ClosedRow {
      fields: row.fields,
      values,
    })
  }

  fn substitute_row(&mut self, row: Row) -> SubstOut<Row> {
    match row {
      Row::Open(var) => {
        let root = self.row_unification_table.find(var);
        match self.row_unification_table.probe_value(root) {
          Some(row) => self.substitute_closedrow(row).map(Row::Closed),
          None => SubstOut::new(Row::Open(root)).with_unbound_row(root),
        }
      }
      Row::Closed(row) => self.substitute_closedrow(row).map(Row::Closed),
    }
  }

  pub fn substitute_ty(&mut self, ty: Type) -> SubstOut<Type> {
        match ty {
            Type::Num => SubstOut::new(Type::Num),
            Type::Unifier(v) => {
                let root = self.unification_table.find(v);
                match self.unification_table.probe_value(root) {
                    Some(ty) => self.substitute_ty(ty),
                    None => {
                        SubstOut::new(Type::Unifier(root)).with_unbound_ty(root)
                    }
                }
            }
                  Type::Func(arg, ret) => {
        let arg_out = self.substitute_ty(*arg);
        let ret_out = self.substitute_ty(*ret);
        arg_out.merge(ret_out, |arg, ret| Type::Func(arg.into(), ret.into()))
      }
      Type::Label(field, value) => self.substitute_ty(*value).map(|ty| Type::Label(field, ty.into())),
      Type::Prod(row) => self.substitute_row(row).map(Type::Prod),
      Type::Sum(row) => self.substitute_row(row).map(Type::Sum),

            _ => todo!(),
        }
    }

    pub fn substitute_ast(
        &mut self,
        ast: Spanned<Intern<Expr<Typed>>>,
    ) -> SubstOut<Spanned<Intern<Expr<Typed>>>> {
        let id = ast.1;
        match *ast.0 {
            Expr::Ident(v) => {
                self.substitute_ty(v.1).map(|ty| Spanned(Expr::Ident(Typed(v.0, ty)).into(), id))
                
            }
            Expr::Number(i) => SubstOut::new(Spanned(Expr::Number(i).into(), id)),
            Expr::Hole(v) => {
                self.substitute_ty(v.1).map(|ty| Spanned(Expr::Hole(Typed(v.0, ty)).into(), id))
            }
            Expr::Lambda(arg, body, is_anon) => {
                self.substitute_ty(arg.1).map(|ty| Typed(arg.0, ty)).merge(self.substitute_ast(body), |arg, body| {
                    
                        Spanned(Expr::Lambda(arg, body, is_anon).into(), id)
                    
                })
                
            }
            Expr::Call(fun, arg) => {
                self.substitute_ast(fun).merge(self.substitute_ast(arg), |fun, arg| Spanned(Expr::Call(fun, arg).into(), id))
                
            }
            _ => todo!(),
        }
    }

  pub fn substitute_row_comb(&mut self, comb: RowCombination) -> SubstOut<Evidence> {
    self
      .substitute_row(comb.left)
      .merge(self.substitute_row(comb.right), |l, r| (l, r))
      .merge(self.substitute_row(comb.goal), |(left, right), goal| {
        Evidence::RowEquation { left, right, goal }
      })
  }
}