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

Command-line arguments are self-explaining, but the `--graph "(String,Int)"` requires some explanations:
We test functions against standards graphs, and they are built with ten-powers vertex (plus one). the Int supplied is the upper-bound of the ten-powers. So `"(Path,100)"` will generate the `Path` with `2`, `11` and `101` vertices. We add a vertex, so even the smallest graph (a Path with 2 vertices) contains an edge.
You can specify several graphs

The default is: `[("Mesh",3),("Clique",2)]`

#### Graphs name

The following graphs are supported:

* Path
* Circuit
* Mesh
* Complete
* Clique

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
  { name :: String
  , desc :: String
  , algorithm :: i -> g -> o
  , inputs    :: Edges -> [Named i] }

```

* `name` _identify_ the benchmarked function. Same function from different libraries (for example `hasEdge`) will be given the same `name` to allow comparison.
* `desc` describe the Suite, will be used only in the output.
* `algorithm` is the actual function to be benchmarked. It take an argument, a graph, and produce something.
* `inputs` will receive the actual graph, and will be used to generate inputs for the algorithm.

### BenchGraph.Suites

This module defines common builder for `Suite`, and particularly provide a stable name for standard operations on graphs, and thus allow simpler comparison (remember, benchmarks are identified by their _name_).

### And if I don't care and only want to add a benchmark ?

The main function for `Criterion` is:
```Haskell
allBench :: (GraphImpl g, NFData g) => [(String,Int)] -> Suite g -> Benchmark
```

It takes a list of graphs names and their size, a suite, and produce a benchmark.

The main function for `Weigh` is very similar:
```Haskell
allWeigh :: (GraphImpl g, NFData g) => Suite g -> Weigh ()
```

It use the default graphs and return the correct `Weigh` monad.

You can always see the living code inside the `benchs/` directory.
