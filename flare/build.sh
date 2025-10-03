#! /usr/bin/bash
if [$1 == "o"]; then 
    cargo run --profile release -- -c examples/ntest.flr

elif [ $1 == "h"]; then
    echo "help"
else 
    cargo run -- -c examples/ntest.flr
fi
#if cargo run -- -c examples/ntest.flr; then
#    qbe -o ntest.s examples/ntest.flr.ssa && gcc -Wall -g -o ntest ntest.s && ./ntest
#else
#    echo "Build Failed";
#fi
#echo "Exit Code:" $?