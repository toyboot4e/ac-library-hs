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
build *args:
    cabal build {{args}}

[private]
alias b := build

# shows all warnings and errors
check:
    cabal build --ghc-options="-fforce-recomp -fno-code"

[private]
alias c := check

# builds the library with Core output
core *args:
    cabal build --ghc-options='-ddump-to-file -ddump-prep' {{args}}

# generates Haddock document
doc *args:
    cabal haddock {{args}}

[private]
alias d := doc

# runs doctest
doctest *args:
    cabal repl --with-ghc=doctest --repl-options='-w -Wdefault' {{args}}

[private]
alias dt := doctest

# runs doctest for modules selected with fzf (multi-select with Tab)
doctest-fzf *args:
    #!/usr/bin/env bash
    IFS=$'\n'
    files="$(fd -e hs . src/ | fzf -m {{args}})"
    echo "Target modules:"
    echo "$files" | sed 's/^/- /'
    # --repl-options=fileA\n--repl-options=fileB
    opts=$(printf -- '--repl-options=%s\n' $files)
    cabal repl --with-ghc=doctest --repl-options='-w -Wdefault' --repl-no-load $opts

[private]
alias dtf := doctest-fzf

# runs the lazysegtree example
eg:
    cabal run example-lazy-segtree

# runs treefmt
format:
    nix fmt

[private]
alias fmt := format

# runs cabal-gild
format-cabal:
    #!/usr/bin/env bash
    TEMP_FILE=$(mktemp)
    cat ac-library-hs.cabal | cabal-gild > $TEMP_FILE
    cat $TEMP_FILE > ac-library-hs.cabal
    rm $TEMP_FILE
    # treefmt .

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

# runs local test for modules selected with fzf (multi-select with Tab)
test-fzf *args:
    #!/usr/bin/env bash
    IFS=$'\n'
    modules=$(fd -e hs . test/ | fzf -m {{args}})
    echo "Target modules:"
    echo "$modules" | sed 's/^/- /'
    # Extract test group names from file paths: test/Tests/Extra/Vector.hs -> Extra.Vector
    patterns=$(echo "$modules" | sed 's|^test/Tests/||; s|\.hs$||; s|/|.|g')
    filter=$(echo "$patterns" | sed 's|.*|/&/|' | paste -sd '\|\|' -)
    cabal test --enable-tests --test-options "-p \"$filter\""

[private]
alias tf := test-fzf

# runs local test a large number of QuickCheck tests
many-test opts='':
    cabal test --enable-tests --test-options '--quickcheck-tests 1000 {{opts}}'

[private]
alias mt := many-test

# runs local test a large number of QuickCheck tests
many-many-test opts='':
    cabal test --enable-tests --test-options '--quickcheck-tests 10000 {{opts}}'

[private]
alias mmt := many-many-test

# runs local test a large number of QuickCheck tests
mmmt opts='':
    cabal test --enable-tests --test-options '--quickcheck-tests 100000 {{opts}}'

# touches all the verification source files
touch:
    touch verify/app/*

[private]
alias to := touch

# runs local test for a online judge problem
verify:
    #!/usr/bin/env bash
    cd verify
    file="$(basename "$(ls app/*.hs | fzf --history .fzf-history)")"
    touch "app/$file"
    competitive-verifier oj-resolve --config .competitive-verifier/config.toml --include "app/$file" > /tmp/cv-resolve.json
    competitive-verifier verify --check-error --verify-json /tmp/cv-resolve.json --tle 30

[private]
alias v := verify

# runs local test for all of the online judge problems
verify-all:
    #!/usr/bin/env bash
    cd verify
    touch app/*
    rm /tmp/cv-result-*.json
    competitive-verifier oj-resolve --config .competitive-verifier/config.toml --include "app/" > /tmp/cv-resolve.json
    # Download test cases sequentially (parallel verify uses --no-download)
    competitive-verifier download --verify-json /tmp/cv-resolve.json
    # Run in parallel
    n=$(nproc)
    for i in $(seq 0 $((n-1))); do
      competitive-verifier verify --no-download --verify-json /tmp/cv-resolve.json --tle 30 \
        --split $n --split-index $i -o /tmp/cv-result-$i.json &
    done
    wait
    # Print summary and failed cases
    jq -s --raw-output '
      [.[].files | to_entries[] | {key, status: .value.verifications[].status}] as $all |
      ($all | group_by(.status) | map({(.[0].status): length}) | add // {}) as $c |
      ($all | map(select(.status == "failure") | .key) | unique) as $failed |
      "success: \($c.success // 0) / failure: \($c.failure // 0) / skipped: \($c.skipped // 0)",
      if ($failed | length) > 0 then
        "Failed:", ($failed[] | "  " + .)
      else empty end
    ' /tmp/cv-result-*.json
    jq -s -e '[.[].files[].verifications[] | select(.status == "failure")] | length == 0' /tmp/cv-result-*.json > /dev/null

 vap := verify-all-parallel
[private]
alias va := verify-all

# runs all of the local tests
test-all:
    cabal build && just test && just doctest && just verify-all

[private]
alias ta := test-all

# runs tests and outputs hpc test coverage
coverage *args:
    cabal test ---enable-coverage {{args}}

# shows where GHC dependencies are installed (are they at `/nix/store`?)
locate-deps:
    ghc-pkg field '*' library-dirs
