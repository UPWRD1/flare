#!/bin/sh
cargo run --release -- -c test.alg
gcc test.alg.c
./a.out