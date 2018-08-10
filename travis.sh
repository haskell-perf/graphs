#!/bin/sh

stack --no-terminal bench --no-run-benchmarks --flag "bench-graph:-reallife"

PREFIX=.stack-work/dist/x86_64-linux/Cabal-2.2.0.1/build

if [ $DO = SPACE ]; then $PREFIX/space/space; else $PREFIX/time/time run --part $STEP --of 3  -g '("Mesh",2)' -g '("Clique",2)' ; fi;
