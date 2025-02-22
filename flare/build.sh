#! /usr/bin/bash
cargo run -- -c examples/ntest.flr
qbe -o ntest.s examples/ntest.flr.ssa && gcc -o ntest ntest.s && ./ntest
# echo $?