use petgraph::prelude::*;

use flare_internals::{
    passes::midend::environment::Environment,
    resource::rep::{
        entry::{Item, PackageEntry},
        quantifier::QualifierFragment,
    },
};

use internment::Intern;
use petgraph::prelude::*;

#[allow(clippy::disallowed_names)]
fn make_graph() -> Environment {
    let mut graph: DiGraph<Item, QualifierFragment> = DiGraph::new();
    let root = graph.add_node(Item::Root);
    let lib_foo = graph.add_node(Item::Dummy("libFoo"));
    let foo = graph.add_node(Item::Dummy("foo"));
    let lib_bar = graph.add_node(Item::Dummy("libBar"));
    let bar = graph.add_node(Item::Dummy("Bar"));
    let baz = graph.add_node(Item::Dummy("baz"));
    let bar_foo = graph.add_node(Item::Dummy("fooooo"));
    graph.extend_with_edges([
        (
            root,
            lib_foo,
            QualifierFragment::Package(Intern::from_ref("Foo")),
        ),
        (
            lib_foo,
            foo,
            QualifierFragment::Func(Intern::from_ref("foo")),
        ),
        (
            root,
            lib_bar,
            QualifierFragment::Package(Intern::from_ref("Bar")),
        ),
        (
            lib_bar,
            bar,
            QualifierFragment::Type(Intern::from_ref("Bar")),
        ),
        (bar, baz, QualifierFragment::Field(Intern::from_ref("f1"))),
        (
            lib_bar,
            bar_foo,
            QualifierFragment::Func(Intern::from_ref("foo")),
        ),
    ]);

    Environment { graph, root }
}

// #[test]
// fn exists() {
//     let e = make_graph();
//     assert!(e
//         .get(&quantifier!(Root, Package("Foo"), Func("foo"), End).into_simple())
//         .is_some())
// }

#[test]
fn get_node_exists() {
    let e = make_graph();
    let search1 = e.get_node(
        &QualifierFragment::Func(Intern::from_ref("foo")),
        &QualifierFragment::Package(Intern::from_ref("Foo")),
    );
    let search2 = e.get_node(
        &QualifierFragment::Type(Intern::from_ref("Bar")),
        &QualifierFragment::Package(Intern::from_ref("Bar")),
    );

    assert!(search1.is_some());
    assert!(search2.is_some());
}

#[test]
fn get_node_dne() {
    let e = make_graph();
    let found = e.get_node(
        &QualifierFragment::Func(Intern::from_ref("Bar")),
        &QualifierFragment::Package(Intern::from_ref("libFoo")),
    );
    assert!(found.is_none());
}

#[test]
fn get_paths() {
    let e = make_graph();
    let foo = Intern::from_ref("foo");
    let res = e.search_for_edge(&QualifierFragment::Func(foo));
    assert_eq!(
        res,
        vec![
            vec![
                QualifierFragment::Package(Intern::from_ref("Foo")),
                QualifierFragment::Func(foo)
            ],
            vec![
                QualifierFragment::Package(Intern::from_ref("Bar")),
                QualifierFragment::Func(foo)
            ]
        ]
    );
}
