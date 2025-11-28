use std::path::{Path, PathBuf};
use std::sync::LazyLock;

use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use flare_internals::passes::midend::typing::Solver;
use flare_internals::*;
use flare_internals::{
    passes::backend::c::C, passes::midend::environment::Environment, resource::rep::ast::Program,
};
use internment::Intern;

static TEST_FILE: LazyLock<&'static Path> = LazyLock::new(|| {
    let relative_path = PathBuf::from("benches/bench_code/bench1.flr");
    let mut absolute_path = std::env::current_dir().unwrap();
    absolute_path.push(relative_path);
    absolute_path.leak()
});

pub fn main() {
    todo!()
}

// pub fn typechecking_bench(c: &mut Criterion) {
//     //let path: &'static Path = PathBuf::from(TEST_FILE).read_dir().canonicalize().unwrap().leak();
//     let id: u64 = 0;
//     let mut ctx = Context::new(*TEST_FILE, id, C);

//     let pack = ctx.parse_file(id).unwrap();
//     let processed = vec![(pack, id)];
//     let program = Program {
//         packages: processed,
//     };

//     //dbg!(program.clone());
//     //dbg!(program.clone());

//     let main = Intern::from_ref("Main");
//     let e = Environment::build(&program).unwrap();
//     let mut tc = Solver::default();
//     c.bench_function("type_check", |b| b.iter(|| black_box(tc.check_item())));
//     //c.bench_function("fib 20", |b| b.iter(|| flare::passes::midend::typechecking::(black_box(20))));
// }

// pub fn master_bench(c: &mut Criterion) {
//     let id: u64 = 0;

//     let mut ctx = Context::new(*TEST_FILE, id, C);

//     //dbg!(program.clone());
//     //dbg!(program.clone());
//     c.bench_function("master_bench", |b| {
//         b.iter(|| black_box(ctx.compile_program(id)))
//     });
//     //c.bench_function("fib 20", |b| b.iter(|| flare::passes::midend::typechecking::(black_box(20))));
// } c

// criterion_group!(
//     master_benches,
//     env_build_bench,
//     typechecking_bench,
//     master_bench
// );
// criterion_main!(master_benches);
