#!/usr/bin/env bash

build () {
  stack build \
    --ghc-options "-optc-static -optl-static" \
    --flag amuletml:amc-prove-server
}

build
rm -rfv result/
mkdir -p result/

for arg in $*; do
  cp ".stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.0.0/build/$arg/$arg" result/
done

upx result/*
