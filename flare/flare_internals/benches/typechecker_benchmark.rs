use std::path::PathBuf;

use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use flare_internals::*;
use flare_internals::{passes::midend::environment::Environment, resource::rep::Program};

const TEST_FILE: &str = "/workspaces/allegro/flare/flare_internals/benches/bench_code/bench1.flr";

pub fn typechecking_bench(c: &mut Criterion) {
    let path = PathBuf::from(TEST_FILE).canonicalize().unwrap();
    let parent_dir = path.parent().unwrap();
    let dir_contents = std::fs::read_dir(parent_dir)
        .unwrap()
        .filter_map(Result::ok)
        .filter(|entry| entry.path().extension().map_or(false, |ext| ext == "flr"))
        .collect::<Vec<_>>();

    let id: u64 = 0;
    let ctx = Context::new(&path, id );

    let processed = dir_contents.iter().map(|entry| {
        let file_path = entry.path();
        let (pack, str) = parse_file(&ctx, id)
            
            .unwrap();
        (pack, file_path, str)
    });
    let program = Program {
        packages: processed.collect(),
    };

    //dbg!(program.clone());
    //dbg!(program.clone());
    let e = Environment::build(program.clone()).unwrap();
    c.bench_function("type_check", |b| b.iter(|| black_box(e.check())));
    //c.bench_function("fib 20", |b| b.iter(|| flare::passes::midend::typechecking::(black_box(20))));
}

pub fn env_build_bench(c: &mut Criterion) {
    let path = PathBuf::from(TEST_FILE).canonicalize().unwrap();
    let parent_dir = path.parent().unwrap();
    let dir_contents = std::fs::read_dir(parent_dir)
        .unwrap()
        .filter_map(Result::ok)
        .filter(|entry| entry.path().extension().map_or(false, |ext| ext == "flr"))
        .collect::<Vec<_>>();
    let id: u64 = 0;
    let ctx = Context::new(&path, id);


    let processed = dir_contents.iter().map(|entry| {
        let file_path = entry.path();
        let (pack, str) = parse_file(&ctx, id)
            
            .unwrap();
        (pack, file_path, str)
    });
    let program = Program {
        packages: processed.collect(),
    };

    //dbg!(program.clone());
    //dbg!(program.clone());
    let mut group = c.benchmark_group("env-throughput");
    group.throughput(Throughput::Elements(program.packages.len() as u64));

    group.bench_function("env_build", |b| {
        b.iter(|| black_box(Environment::build(program.clone()).unwrap()))
    });
    group.finish();
    //c.bench_function("fib 20", |b| b.iter(|| flare::passes::midend::typechecking::(black_box(20))));
}

pub fn master_bench(c: &mut Criterion) {
    let path = PathBuf::from(TEST_FILE).canonicalize().unwrap();
        let id: u64 = 0;

    let ctx = Context::new(&path, id);

    //dbg!(program.clone());
    //dbg!(program.clone());
    c.bench_function("master_bench", |b| {
        b.iter(|| black_box(flare_internals::compile_program(&ctx, id)))
    });
    //c.bench_function("fib 20", |b| b.iter(|| flare::passes::midend::typechecking::(black_box(20))));
}

criterion_group!(benches, env_build_bench, typechecking_bench, master_bench);
criterion_main!(benches);
