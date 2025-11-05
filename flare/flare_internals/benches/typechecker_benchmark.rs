use std::path::{Path, PathBuf};
use std::sync::LazyLock;

use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use flare_internals::resource::rep::ast::Package;
use flare_internals::*;
use flare_internals::{passes::midend::environment::Environment, resource::rep::ast::Program};

static TEST_FILE: LazyLock<&'static Path> = LazyLock::new(|| {
    let relative_path = PathBuf::from("benches/bench_code/bench1.flr");
    let mut absolute_path = std::env::current_dir().unwrap();
    absolute_path.push(relative_path);
    absolute_path.leak()
});

pub fn typechecking_bench(c: &mut Criterion) {
    //let path: &'static Path = PathBuf::from(TEST_FILE).read_dir().canonicalize().unwrap().leak();
    let parent_dir = TEST_FILE.parent().unwrap();

    let dir_contents = std::fs::read_dir(parent_dir)
        .unwrap()
        .filter_map(Result::ok)
        .filter(|entry| entry.path().extension().is_some_and(|ext| ext == "flr"))
        .collect::<Vec<_>>();

    let id: u64 = 0;
    let ctx = Context::new(*TEST_FILE, id);

    let processed = dir_contents.iter().map(|entry| {
        let file_path: &'static Path = entry.path().leak();
        let (pack, str) = ctx.parse_file(id).unwrap();
        (pack, file_path, str)
    });
    let program = Program {
        packages: processed.collect::<Vec<_>>(),
    };

    //dbg!(program.clone());
    //dbg!(program.clone());
    let e = Environment::build(&program).unwrap();
    c.bench_function("type_check", |b| b.iter(|| black_box(e.check())));
    //c.bench_function("fib 20", |b| b.iter(|| flare::passes::midend::typechecking::(black_box(20))));
}

pub fn env_build_bench(c: &mut Criterion) {
    let parent_dir = TEST_FILE.parent().unwrap();
    let dir_contents = std::fs::read_dir(parent_dir)
        .unwrap()
        .filter_map(Result::ok)
        .filter(|entry| entry.path().extension().is_some_and(|ext| ext == "flr"))
        .collect::<Vec<_>>();
    let id: u64 = 0;
    let ctx = Context::new(*TEST_FILE, id);

    let processed: Vec<(Package, &Path, &str)> = dir_contents
        .iter()
        .map(|entry| {
            let file_path: &'static Path = entry.path().leak();
            let (pack, str): (_, &'static str) = ctx.parse_file(id).unwrap();
            (pack, file_path, str)
        })
        .collect();
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

    let mut ctx = Context::new(*TEST_FILE, id);

    //dbg!(program.clone());
    //dbg!(program.clone());
    c.bench_function("master_bench", |b| {
        b.iter(|| black_box(ctx.compile_program(id)))
    });
    //c.bench_function("fib 20", |b| b.iter(|| flare::passes::midend::typechecking::(black_box(20))));
}

criterion_group!(benches, env_build_bench, typechecking_bench, master_bench);
criterion_main!(benches);
