# bench-graph

## Warning: Under Active Developement
This is being developed for the Google Summer of Code 2018.
Please do not except anything from the code for now. 

Feel free to open an issue anyway :)

## Results
Current results of `cabal bench time` and `cabal bench space` can be found here: https://travis-ci.org/haskell-perf/graphs

## Usage

### Time
The benchmark suite `time` will run all queued benchmark using `Criterion`.

To run benchmarks, use `time run`. It came with several options to customize the run (you can select libraries to compare, etc...).

To list benchmarks, use `time list`.

### Space

The benchmark suite `space` will run all queued benchmark using `Weigh`.

To run benchmarks, use `space run`. It came with several options to customiez the run (you can select libraries to compare, etc...).
Please note that the benchmarks will all run before any print, so it can takes a long time before anything is print on-screen, mostly with big graphs.

To list benchmarks, use `space list`.

### Data-Size
The benchmark suite `datasize` will use `ghc-datasize` to calculate size of graphs

### Arguments

Command-line arguments are self-explaining, but the `--graphs-size (int,int,int)` requires some explanations:
We test functions against standards graphs, and they are built from ten powers. For example, a function will be tested with a path of length `1`, then `10`, then `100`. The ints represents these powers. The first is for _path_, the second for _circuit_ and the last for _complete_ graphs. Please be careful with this last one, because it can become _really big_.

## About implementation

### The `GraphImpl` class

All libraries are required to provide a `GraphImpl` instance:
```Haskell
type Edges = [(Int,Int)]

class GraphImpl g where
    mkGraph :: Edges -> g
```

It allows to convert `Edges` ( a traditional list of edges ) to the graph representation of the library.

### The Named data

```Haskell
data Named a = Named String a
```
`Named a` data is used to allow a lighter syntax, and can be viewed as a `(String,a)`

### The `Suite g` data

All functions to bench are encapsulated inside a `Suite` data:
```Haskell
data Suite g = forall i o. NFData o => Suite
    { algorithm :: i -> g -> o
    , inputs    :: Edges -> [Named i] }

type NSuite g = Named (Suite g)
```

* `suiteName` _identify_ the benchmarked function. Same function from different libraries (for example `hasEdge`) will be given the same `name` to allow comparison.
* `algorithm` is the actual function to be benchmarked. It take an argument, a graph, and produce something.
* `inputs` will receive the actual graph, and will be used to generate inputs for the algorithm.

### BenchGraph.Suites

This module defines common builder for `Suites`, and particularly provide a stable name for standard operations on graphs, and thus allow simpler comparison (remember, benchmarks are identified by their _name_).

### And if I don't care and only want to add a benchmark ?

The main function for `Criterion` is:
```Haskell
allBenchs :: (GraphImpl g, NFData g) => (Int,Int,Int) -> [Suite g] -> [Benchmark]
```

It takes size arguments for graphs, a list of suite, and produce a list of benchmark.

The main function for `Weigh` is very similar:
```Haskell
allWeighs :: (GraphImpl g, NFData g) => [Suite g] -> Weigh ()
```

It use the default size arguments ( `(3,3,2)` ) and return the correct `Weigh` monad.

You can always see the living code inside the `benchs/` directory.
