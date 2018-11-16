# Mistakenly Mocking MTL

(Ab)using Typeclasses in Haskell to Model and Mock Effects

## Prerequisites

Install the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)

From the project root, run:
    
    stack test --fast
    
...which will download and set up GHC, retrieve package dependencies, build the
project, and run the test suite (with `-O0` optimizations to speed things up).

## Tooling

I've included a `Makefile` with this project that captures a lot of my common
development flows within a Haskell project.

Since some of these commands are dependent on 
[ghcid](https://github.com/ndmitchell/ghcid), so before running any of them
make sure its installed and available on your path with `stack install ghcid`.

To build the project quickly:

    make build-fast

To enter the project REPL: 

    make ghci

To run `ghcid`, which will recompile the project on changes and output any type
errors to the console:

    make ghcid

To run `ghcid` and have it rerun the test suite after the project successfully
type checks:

    make ghcid-test

To run the test suite on its own:

    make test
