
#![allow(unused)]
fn main() {
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use flare_internals::*;

pub fn criterion_benchmark(c: &mut Criterion) {
    todo!()
    //c.bench_function("fib 20", |b| b.iter(|| flare::passes::midend::typechecking::(black_box(20))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
}
