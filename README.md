[![Build](https://github.com/alessandrocandolini/advent-of-code2025/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/advent-of-code2025/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/advent-of-code2025/graph/badge.svg?token=yDHcPy0Gtx)](https://codecov.io/gh/alessandrocandolini/advent-of-code2025) [![Completion Status](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/alessandrocandolini/advent-of-code2025/main/.github/badges/completion.json)](https://adventofcode.com/2025)


# advent-of-code2025

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/). The recommended way to install stack is by using [ghcup](https://www.haskell.org/ghcup/), although it's also possible to use [the nix package manager](https://nixos.org/).

Assuming `stack` is installed in the system, to **build** the project use
```
stack build
```
To **build and run the tests**, run
```
stack test
```
which is equivalent to
```
stack build --test
```
For **faster feedback loop** during development, it's possible to run tests continuously on every file change:
```
stack test --fast --file-watch
```
To run tests with **test coverage** instrumentation,
```
stack test --coverage
```
which generates a textual and HTML report. Tests are automatically run in the CI and test coverage reports are uploaded to codecov.

To **run the executable via stack**,
```
stack exec aoc2025
```
or passing arguments
```
stack exec aoc2025 -- solve -d <day> -f <filename>
```
To run the **benchmarks**
```
stack bench --benchmark-arguments="--output report.html"
```
which generates a `report.html` HTML report.
Benchmarks are NOT run as part of the CI, to keep the CI fast.

To **install** the executable under `~/.local/bin`,
```
stack install
```
and the executable can be run with `aoc2025` or passing arguments like
```
aoc2025 solve -d 1 -f inputs/day1
```
assuming `~/.local/bin` is in the `$PATH` variable.

To get more insights into runtime performance, it's possible to extract information from the Haskell RTS:
```
aoc2025 -v solve -d 1 --with-input +RTS -s -N1 < <(pbpaste)
```

To run a version of **ghci** compatible with the resolver
```
stack ghci
```
For more information, refer to the `stack` official docs.


## Available commands

Thanks to [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative), the CLI automatically generates documentation from code. It's recommended to use the generated helper to explore all the options. However, a summary is provided here of the most relevant options.


### Run solutions

From **file**:
```
aoc2025 solve -d 1 -f inputs/day1
```

From **standard input**:
```
aoc2025 solve -d 1  --with-input < inputs/day1
```
or
```
cat input/day1 | aoc2025 solve -d 1 --with-input
```

From **pbcopy** (on macos)
```
aoc2025 solve -d 1 --with-input < <(pbpaste)
```

### Retrieve stats
```
export AOC_SESSION=<insert the cookie value>
aoc2025 stats
aoc2025 stats -y 2025
aoc2025 stats -y 2025 --json
```
