# bench-graph

## Warning: Under Active Developement
This is beign developed for the Google Summer of Code 2018.
Please do not except anything from the code for now. 

Feel free to open an issue anyway :)

## Results
Current results of `cabal bench time` and `cabal bench space` can be found here: https://travis-ci.org/haskell-perf/graphs

## Usage of Time
The time-comparing tool can be used with:
```Bash
$ cabal bench time --benchmark-option=run
```

### List
The `list` command will show all the benchmarks:
```Bash
$ cabal bench time --benchmark-option=list
```

### Run
The `run` function actually run benchmarks.
```Bash
Usage: time run ([--part ARG] [--of ARG] | [-o|--only NAME]) [-s|--summarize]
```

## Usage of Space
The space-comparing tool can be used with:
```Bash
$ cabal bench space --benchmark-option=run
```

### List
The `list` command will show all the benchmarks:
```Bash
$ cabal bench space --benchmark-option=list
```

### Run
The `run` function actually run benchmarks.
```Bash
Usage: space run [-o|--only NAME] [-s|--summarize]
```

