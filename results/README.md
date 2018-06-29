# Introduction

In this folder, you will find the lastest benchmarks of 4 haskell graphs libraries:

* [Alga (algebraic-graphs) v0.2.0.0 (not yet on hackage)](https://github.com/snowleopard/alga)
* [Data.Graph from containers v0.5.11.0](https://hackage.haskell.org/package/containers-0.5.11.0)
* [FGL v5.6.0.0](https://hackage.haskell.org/package/fgl-5.6.0.0)
* [Hash-Graph (not yet on hackage)](https://github.com/patrickdoc/hash-graph)

The benchmarks were realised using `Stack` and the `stack.newest.yaml` configuration

## Tools

* Time benchmarks were run using [Criterion v1.4.1.0](https://hackage.haskell.org/package/criterion-1.4.0.0) 

* ~~Space benchmarks were run using [Weigh v0.12](https://hackage.haskell.org/package/weigh-0.0.12)~~

* ~~Data size benchmarks were run using [ghc-datasize v0.2.0](http://hackage.haskell.org/package/ghc-datasize-0.2.0)~~

## Results

* Time: https://github.com/haskell-perf/graphs/blob/master/results/TIME.md
* Space: https://github.com/haskell-perf/graphs/blob/master/results/SPACE.md

Note: Some functions of Data.Graph was not in the original library (edgeCount, hasEdge, vertexCount), their defintion can be found in `bench/Containers/Graph.hs`

## Some words about graphs
The functions are benchmarked against two generic graphs:

* A [mesh](https://en.wikipedia.org/wiki/Lattice_graph): This graph can be represented as a regular tiling of a plane, with edges going only right or up. It is a good example of a sparse graph. For example a mesh with nine vertices can be viewed as:

| | | |  | | 
| :---: | :---: | :---: | :---: | :---: |
| 6 | &#8594; | 7 | &#8594; | 8
| &#8593; | | &#8593; | |  &#8593;
| 3 | &#8594; | 4 | &#8594; | 5
| &#8593; | | &#8593; | |  &#8593;
| 1 | &#8594; | 1 | &#8594; | 2

* A [clique](https://en.wikipedia.org/wiki/Clique_(graph_theory)): This a graph such as every two distinct vertices are adjacent. It is a good example of a dense graph. The list of the edges of a clique with 5 vertices will look like:
```
[(0,1),(0,2),(0,3),(0,4),(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
```

* RealLife graphs are graphs from the real life, please see [https://github.com/haskell-perf/graphs/tree/master/src/BenchGraph/RealLife/Graphs]

Graphs are built with successive ten powers vertices. Here, with 1, 10, 100 and 1000 vertices.

## About arguments
All the functions are tested with arguments in the _domain_ of the graph, where applicable: unless it is mentioned, edges and vertices generated for the test can be in the complete graph with the same number of vertices.

## Remarks

* Results are condensed in table, without the detail of arguments. For more precise results, please run the benchmark suite
* Other graphs are supported (path, circuit and complete), you can run the benchmark suite using them

