use crate::root::resource::{ast::*, itypes::Itype};

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Expr(Expr),
    Var(String),
    Num,
    Bool,
    Str,
    Mute,
    Arrow(ArrowType)
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrowType {
    domain: Vec<Term>,
    range: Box<Term>,
}


impl Term {
    fn is_ident(&self) -> bool {
        matches!(self, Term::Expr(_) | Term::Var(_))
    }

    pub fn make_arrow(domain: Vec<Term>, range: Term) -> Self {
        Term::Arrow(ArrowType {
            domain,
            range: Box::new(range),
        })
    }
}


#[derive(Clone, Debug)]
pub struct Substitution {
    var: Term,
    is: Term,
}

impl Substitution {
    pub fn new(var: Term, is: Term) -> Self {
        Self { var, is }
    }
}

#[derive(Debug, Clone)]
pub struct Constraint {
    lhs: Term,
    rhs: Term,
}

impl Constraint {
    fn new(lhs: Term, rhs: Term) -> Self {
        Constraint { lhs, rhs }
    }
}

pub fn infer_types(expr: &Expr) -> Vec<Substitution> {
    let mut cons = vec![];
    generate_constraints(expr, &mut cons);
    // for c in &cons {
    //     println!("Constraint: {c:?}")
    // }
    unify(&mut cons, &mut vec![])
}

fn generate_constraints(expr: &Expr, constraints: &mut Vec<Constraint>) {
    match expr {
        Expr::Scalar(s) => {
            // When the expression is a number, we expect the type
            // of the expression to be numeric:
            match s {
                Itype::Mute => constraints.push(Constraint {
                    lhs: Term::Expr(expr.clone()),
                    rhs: Term::Mute,
                }),
                Itype::Int(_) => constraints.push(Constraint {
                    lhs: Term::Expr(expr.clone()),
                    rhs: Term::Num,
                }),
                Itype::Flt(_) => constraints.push(Constraint {
                    lhs: Term::Expr(expr.clone()),
                    rhs: Term::Num,
                }),
                Itype::Str(_) => constraints.push(Constraint {
                    lhs: Term::Expr(expr.clone()),
                    rhs: Term::Str,
                }),
                Itype::Bool(_) => constraints.push(Constraint {
                    lhs: Term::Expr(expr.clone()),
                    rhs: Term::Bool,
                }),
            }
            ;
        }
        Expr::Variable(s) => {
            constraints.push(Constraint {
                lhs: Term::Expr(expr.clone()),
                rhs: Term::Var(s.clone()),
            });
        }
        Expr::BinaryOp(op,args) => {
            let (left, right) = *args.clone();
            generate_constraints(&left, constraints);
            generate_constraints(&right, constraints);
            let consequent = vec![
                Constraint::new(Term::Expr(left.clone()), Term::Num),
                Constraint::new(Term::Expr(right.clone()), Term::Num),
                Constraint::new(Term::Expr(expr.clone()), Term::Num),
            ];
            constraints.extend(consequent);
        },

        Expr::Call {name, on, args} => {
            generate_constraints(&Expr::Variable(name.to_string()), constraints);
            for arg in args {
                generate_constraints(&arg, constraints);
            }
            let mut new_domain: Vec<Term> = vec![];
            for arg in args {
                new_domain.push(Term::Expr(arg.clone()))
            }
            let consequent = vec![Constraint::new(
                Term::Expr(Expr::Variable(name.to_string()).clone()),
                Term::Arrow(ArrowType {
                    domain: new_domain,
                    range: Box::new(Term::Expr(expr.clone())),
                }),
            )];
            constraints.extend(consequent);
        }
        _ => todo!(),
    }
}

fn occurs_check(left: &Term, right: &Term) -> bool {
    match left {
        Term::Arrow(ArrowType { domain, range }) => {
            // occurs_check(left, domain) || occurs_check(left, range)
            occurs_check(left, range)
        }
        _ => left == right,
    }
}

fn replace(left: &Term, term: &Term, right: &Term) -> Term {
    match term {
        Term::Arrow(ArrowType { domain, range }) => {
            let mut ndomain: Vec<Term> = vec![];
            for arg in domain {
                let narg = replace(left, arg, right);
                ndomain.push(narg);
            }
            Term::Arrow(ArrowType {
                domain: ndomain,
                range: Box::new(replace(left, range, right)),
            })
        }
        _ => {
            if left == term {
                right.clone()
            } else {
                term.clone()
            }
        }
    }
}

fn replace_all(
    left: &Term,
    right: &Term,
    consts: &mut [Constraint],
    subst: &mut [Substitution],
) {
    if !occurs_check(left, right) {
        for c in consts.iter_mut() {
            c.lhs = replace(left, &c.lhs, right);
            c.rhs = replace(left, &c.rhs, right);
        }

        for s in subst.iter_mut() {
            s.var = replace(left, &s.var, right);
            s.is = replace(left, &s.is, right);
        }
    } else {
        panic!("Occurs check failed.");
    }
}

fn unify(
    consts: &mut Vec<Constraint>,
    subs: &mut Vec<Substitution>,
) -> Vec<Substitution> {
    if consts.is_empty() {
        subs.to_vec()
    } else {
        let (first, rest) = consts.split_at_mut(1);
        let first = first.first().unwrap();

        let left = first.lhs.clone();
        let right = first.rhs.clone();

        if left == right {
            unify(&mut rest.to_vec(), subs)
        } else if left.is_ident() {
            replace_all(&left, &right, rest, subs);
            subs.push(Substitution::new(left, right));
            unify(&mut rest.to_vec(), subs)
        } else if right.is_ident() {
            replace_all(&right, &left, rest, subs);
            subs.push(Substitution::new(right, left));
            unify(&mut rest.to_vec(), subs)
        } else {
            match (&left, &right) {
                (
                    Term::Arrow(ArrowType {
                        domain: d_one,
                        range: r_one,
                    }),
                    Term::Arrow(ArrowType {
                        domain: d_two,
                        range: r_two,
                    }),
                ) => {
                    let mut new_rest = rest.to_vec();
                    // new_rest.extend(vec![
                    //     Constraint::new(d_one.clone(), d_two.clone()),
                    //     Constraint::new(r_one.clone(), r_two.clone()),
                    // ]);
                    unify(&mut new_rest, subs)
                }
                _ => {
                    for sub in subs {
                        println!("Found: {sub:?}");
                    }
                    let msg = format!("{left:?} and {right:?} do not unify.");
                    panic!("{msg}");
                }
            }
        }
    }
}
