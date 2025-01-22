# Just a task runner
# <https://github.com/casey/just>

# shows this help message
help:
    @just -l

# runs the benchmark
bench:
    cabal bench --enable-tests --benchmark-options='--output a.html'

[private]
alias be := bench

# builds the library
build:
    cabal build

[private]
alias b := build

# shows all warnings and errors
check:
    cabal build --ghc-options="-fforce-recomp -fno-code"

[private]
alias c := check

# generates Haddock document
doc:
    cabal haddock

[private]
alias d := doc

# runs doctest
doctest:
    cabal repl --with-ghc=doctest --repl-options='-w -Wdefault'

[private]
alias dt := doctest

# runs the lazysegtree example
eg:
    cabal run example-lazy-segtree

# rebuilds the project and measures the compile time (nix flakes required)
measure:
    cabal clean && cabal build ac-library-hs --ghc-options "-ddump-to-file -ddump-timings" && nix run nixpkgs#time-ghc-modules

[private]
alias m := measure

# runs local test (parameter example: '-p /SegTree/')
test opts='':
    cabal test --enable-tests --test-options '{{opts}}'

[private]
alias t := test

touch:
    touch verify/app/*

[private]
alias to := touch

# runs local test for a online judge problem
verify:
    cd verify && ./script/verify

[private]
alias v := verify

# runs local test for all of the online judge problems
verify-all:
    cd verify && touch app/* && ./script/verify-all

[private]
alias va := verify-all
