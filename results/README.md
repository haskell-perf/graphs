# Introduction

In this folder, you will find the lastest benchmarks of 4 haskell graphs libraries:

* [Alga (algebraic-graphs) v0.1.1.1](http://hackage.haskell.org/package/algebraic-graphs-0.1.1.1)
* [Data.Graph from containers v0.5.10.2](https://hackage.haskell.org/package/containers-0.5.10.2)
* [FGL v5.6.0.0](https://hackage.haskell.org/package/fgl-5.6.0.0)
* [Hash-Graph (not yet on hackage)](https://github.com/patrickdoc/hash-graph)

## Tools

* Time benchmarks were run using [Criterion v1.4.0.0](https://hackage.haskell.org/package/criterion-1.4.0.0) 

* Space benchmarks were run using [Weigh v0.12](https://hackage.haskell.org/package/weigh-0.0.12)

* Data size benchmarks were run using [ghc-datasize v0.2.0](http://hackage.haskell.org/package/ghc-datasize-0.2.0)

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

Graphs are built with successive ten powers vertices plus one. Here, with 1, 11, 101 and 1001 vertices.

## About arguments
All the functions are tested with arguments in the _domain_ of the graph, where applicable: unless it is mentioned, edges and vertices generated for the test can be in the complete graph with the same number of vertices.

## Remarks

* Results are condensed in table, without the detail of arguments. For more precise results, please run the benchmark suite
* Other graphs are supported (path, circuit and complete), you can run the benchmark suite using them

