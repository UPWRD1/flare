#!/bin/sh
cargo run -- -c test.alg
gcc test.alg.c
./a.out