use std::path::{Path, PathBuf};
use std::sync::LazyLock;

use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use flare_internals::passes::backend::c::C;
use flare_internals::passes::midend::typechecking::Solver;
use flare_internals::*;
use flare_internals::{passes::midend::environment::Environment, resource::rep::ast::Program};
use internment::Intern;

static TEST_FILE: LazyLock<&'static Path> = LazyLock::new(|| {
    let relative_path = PathBuf::from("benches/bench_code/bench1.flr");
    let mut absolute_path = std::env::current_dir().unwrap();
    absolute_path.push(relative_path);
    absolute_path.leak()
});

pub fn typechecking_bench(c: &mut Criterion) {
    //let path: &'static Path = PathBuf::from(TEST_FILE).read_dir().canonicalize().unwrap().leak();
    let id: u64 = 0;
    let mut ctx = Context::new(*TEST_FILE, id, C);

    let pack = ctx.parse_file(id).unwrap();
    let processed = vec![(pack, id)];
    let program = Program {
        packages: processed,
    };

    //dbg!(program.clone());
    //dbg!(program.clone());

    let main = Intern::from_ref("Main");
    let e = Environment::build(&program).unwrap();
    let mut tc = Solver::new(
        &e,
        resource::rep::quantifier::QualifierFragment::Package(main),
    );
    c.bench_function("type_check", |b| b.iter(|| black_box(tc.check())));
    //c.bench_function("fib 20", |b| b.iter(|| flare::passes::midend::typechecking::(black_box(20))));
}

pub fn env_build_bench(c: &mut Criterion) {
    let id: u64 = 0;
    let mut ctx = Context::new(*TEST_FILE, id, C);

    let pack = ctx.parse_file(id).unwrap();
    let processed = vec![(pack, id)];
    let program = Program {
        packages: processed,
    };

    //dbg!(program.clone());
    //dbg!(program.clone());
    let mut group = c.benchmark_group("env-throughput");
    group.throughput(Throughput::Elements(program.packages.len() as u64));

    group.bench_function("env_build", |b| {
        b.iter(|| black_box(Environment::build(&program).unwrap()))
    });
    group.finish();
    //c.bench_function("fib 20", |b| b.iter(|| flare::passes::midend::typechecking::(black_box(20))));
}

pub fn master_bench(c: &mut Criterion) {
    let id: u64 = 0;

    let mut ctx = Context::new(*TEST_FILE, id, C);

    //dbg!(program.clone());
    //dbg!(program.clone());
    c.bench_function("master_bench", |b| {
        b.iter(|| black_box(ctx.compile_program(id)))
    });
    //c.bench_function("fib 20", |b| b.iter(|| flare::passes::midend::typechecking::(black_box(20))));
}

criterion_group!(benches, env_build_bench, typechecking_bench, master_bench);
criterion_main!(benches);
