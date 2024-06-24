#!/bin/sh
llc -filetype=obj ntest.alg.ll -o ntest.o
clang ntest.o -o ntest
./ntest