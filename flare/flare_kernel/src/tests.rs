use super::*;

fn run(store: &mut Store, env: &Env, label: &str, term: &Term) {
    match infer(store, env, term) {
        Ok(ty) => {
            let v = eval::eval(&Rc::new(eval::VEnv::Empty), term);
            println!("{label}");
            println!("  type  = {}", pretty::print_type(store, &ty));
            println!("  value = {}", pretty::print_value(&v));
        }
        Err(e) => panic!("{label}\n  type error: {:?}", e),
    }
}

// Recursive sum-of-products row (mu), an Int list ---
// list = mu X. <Nil: {}, Cons: {head: Int, tail: X}>
#[test]
fn rec_int_list() {
    let mut store = Store::default();
    let env: Env = HashMap::new();
    let cons_row = Row::Extend(
        "head".into(),
        Rc::new(Type::Con("Int".into())),
        Rc::new(Row::Extend(
            "tail".into(),
            Rc::new(Type::SelfRef),
            Rc::new(Row::Empty),
        )),
    );
    let list_row = Row::Extend(
        "Nil".into(),
        Rc::new(Type::Record(Rc::new(Row::Empty))),
        Rc::new(Row::Extend(
            "Cons".into(),
            Rc::new(Type::Record(Rc::new(cons_row))),
            Rc::new(Row::Empty),
        )),
    );
    let list_ty = Type::Mu(Rc::new(Type::Variant(Rc::new(list_row))));

    let nil_term = Term::Fold(
        list_ty.clone(),
        Box::new(Term::Variant("Nil".into(), Box::new(Term::RecordEmpty))),
    );
    let cons1 = Term::Fold(
        list_ty.clone(),
        Box::new(Term::Variant(
            "Cons".into(),
            Box::new(Term::RecordExtend {
                new_label: "head".into(),
                definition: Box::new(Term::Lit(1)),
                rest: Box::new(Term::RecordExtend {
                    new_label: "tail".into(),
                    definition: Box::new(nil_term),
                    rest: Box::new(Term::RecordEmpty),
                }),
            }),
        )),
    );
    run(
        &mut store,
        &env,
        "\nExample 3: build a recursive Int list, fold [1]",
        &cons1,
    );

    // Unfold one layer and pull the head back out.
    let head_term = Term::Case {
        scrutinee: Box::new(Term::Unfold(list_ty.clone(), Box::new(cons1.clone()))),
        branches: vec![(
            "Cons".into(),
            "c".into(),
            Term::RecordSelect(Box::new(Term::Var("c".into())), "head".into()),
        )],
        default: Some(("_other".into(), Box::new(Term::Lit(-1)))),
    };
    run(
        &mut store,
        &env,
        "\nExample 3b: unfold + case to extract head",
        &head_term,
    );
}

//closed sum row (exhaustive case, no default)
#[test]
fn closed_sum_row() {
    let mut store = Store::default();
    let env: Env = HashMap::new();
    let scrut = Term::Variant("Left".into(), Box::new(Term::Lit(10)));
    let case_term = Term::Case {
        scrutinee: Box::new(scrut),
        branches: vec![
            ("Left".into(), "n".into(), Term::Var("n".into())),
            ("Right".into(), "n".into(), Term::Var("n".into())),
        ],
        default: None,
    };
    run(
        &mut store,
        &env,
        "\nExample 2: closed variant case",
        &case_term,
    );
}

// row-polymorphic field projection
// let get_x = \r -> r.x
// in {a = get_x {x=1,y=2}, b = get_x {x=3,z=4}}
#[test]
fn row_polymorphic_field_proj() {
    let mut store = Store::default();
    let env: Env = HashMap::new();

    let get_x = Term::Lam(
        "r".into(),
        Box::new(Term::RecordSelect(
            Box::new(Term::Var("r".into())),
            "x".into(),
        )),
    );
    let rec1 = Term::RecordExtend {
        new_label: "x".into(),
        definition: Box::new(Term::Lit(1)),
        rest: Box::new(Term::RecordExtend {
            new_label: "y".into(),
            definition: Box::new(Term::Lit(2)),
            rest: Box::new(Term::RecordEmpty),
        }),
    };
    let rec2 = Term::RecordExtend {
        new_label: "x".into(),
        definition: Box::new(Term::Lit(3)),
        rest: Box::new(Term::RecordExtend {
            new_label: "z".into(),
            definition: Box::new(Term::Lit(4)),
            rest: Box::new(Term::RecordEmpty),
        }),
    };
    let prog1 = Term::RecordExtend {
        new_label: "get_x".into(),
        definition: Box::new(get_x),
        rest: Box::new(Term::RecordExtend {
            new_label: "a".into(),
            definition: Box::new(Term::App(
                Box::new(Term::Var("get_x".into())),
                Box::new(rec1),
            )),
            rest: Box::new(Term::RecordExtend {
                new_label: "b".into(),
                definition: Box::new(Term::App(
                    Box::new(Term::Var("get_x".into())),
                    Box::new(rec2),
                )),
                rest: Box::new(Term::RecordEmpty),
            }),
        }),
    };
    run(
        &mut store,
        &env,
        "Example 1: row-polymorphic projection",
        &prog1,
    );
}
