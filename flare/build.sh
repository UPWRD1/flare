#! /usr/bin/bash
cargo run -- -c examples/ntest.flr
#if cargo run -- -c examples/ntest.flr; then
#    qbe -o ntest.s examples/ntest.flr.ssa && gcc -Wall -g -o ntest ntest.s && ./ntest
#else
#    echo "Build Failed";
#fi
#echo "Exit Code:" $?