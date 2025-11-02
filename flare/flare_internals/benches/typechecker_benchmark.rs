use std::path::{Path, PathBuf};

use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use flare_internals::resource::rep::ast::Package;
use flare_internals::*;
use flare_internals::{passes::midend::environment::Environment, resource::rep::ast::Program};

const TEST_FILE: &str =
    "/Users/lukedavis/Documents/GitHub/flare/flare/flare_internals/benches/bench_code/bench1.flr";
//    "~/Documents/GitHub/flare/flare/flare_internals/benches/bench_code/bench1.flr";

pub fn typechecking_bench(c: &mut Criterion) {
    let path: &'static Path = PathBuf::from(TEST_FILE).canonicalize().unwrap().leak();
    let parent_dir = path.parent().unwrap();
    let dir_contents = std::fs::read_dir(parent_dir)
        .unwrap()
        .filter_map(Result::ok)
        .filter(|entry| entry.path().extension().is_some_and(|ext| ext == "flr"))
        .collect::<Vec<_>>();

    let id: u64 = 0;
    let ctx = Context::new(path, id);

    let processed = dir_contents.iter().map(|entry| {
        let file_path: &'static Path = entry.path().leak();
        let (pack, str) = parse_file(&ctx, id).unwrap();
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
    let path: &'static Path = PathBuf::from(TEST_FILE).canonicalize().unwrap().leak();
    let parent_dir = path.parent().unwrap();
    let dir_contents = std::fs::read_dir(parent_dir)
        .unwrap()
        .filter_map(Result::ok)
        .filter(|entry| entry.path().extension().is_some_and(|ext| ext == "flr"))
        .collect::<Vec<_>>();
    let id: u64 = 0;
    let ctx = Context::new(path, id);

    let processed: Vec<(Package, &Path, &str)> = dir_contents
        .iter()
        .map(|entry| {
            let file_path: &'static Path = entry.path().leak();
            let (pack, str): (_, &'static str) = parse_file(&ctx, id).unwrap();
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
    let path: &'static Path = PathBuf::from(TEST_FILE).canonicalize().unwrap().leak();
    let id: u64 = 0;

    let mut ctx = Context::new(path, id);

    //dbg!(program.clone());
    //dbg!(program.clone());
    c.bench_function("master_bench", |b| {
        b.iter(|| black_box(flare_internals::compile_program(&mut ctx, id)))
    });
    //c.bench_function("fib 20", |b| b.iter(|| flare::passes::midend::typechecking::(black_box(20))));
}

criterion_group!(benches, env_build_bench, typechecking_bench, master_bench);
criterion_main!(benches);
