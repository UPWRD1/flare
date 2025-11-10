#![cfg(feature = "testing")]
use criterion::{criterion_group, criterion_main, Criterion};
use iai::{black_box, main};

use flare_internals::{
    passes::midend::environment::*,
    resource::rep::{ast::Package, quantifier::QualifierFragment},
};
use internment::Intern;
fn get(c: &mut Criterion) {
    let parent_foo = QualifierFragment::Package(Intern::from_ref("Foo"));
    let func_foo = QualifierFragment::Func(Intern::from_ref("foo"));

    let mut env = Environment::make_graph();

    c.bench_function("get_bench", |b| {
        b.iter(|| criterion::black_box(env.test_get(&[parent_foo, func_foo][..])))
    });
}

// fn iai_get() {
//     let parent_foo = QualifierFragment::Package(Intern::from_ref("Foo"));

//     let func_foo = QualifierFragment::Func(Intern::from_ref("foo"));

//     let mut env = Environment::make_graph();

//     //dbg!(program.clone());
//     //dbg!(program.clone());

//     iai::black_box(env.test_get(&[parent_foo, func_foo][..]));
// }

// iai::main!(iai_get);

criterion_group!(env_benches, get);
criterion_main!(env_benches);
