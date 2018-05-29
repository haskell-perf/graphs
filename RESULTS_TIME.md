# Compare benchmarks

Doing:

----
isEmpty
vertexList
vertexCount
edgeCount
edgeList
hasEdge
add a new edge
add a new vertex
remove a vertex
equality
remove an edge
transpose
make a Mesh from a list
make a Complete from a list
dff
topSort
reachable
merge a context

----

## isEmpty
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         36.79 ns
      </TD>
      <TD CLASS = "thinright">
         45.20 ns
      </TD>
      <TD>
         43.08 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         30.34 ns
      </TD>
      <TD CLASS = "thinright">
         30.87 ns
      </TD>
      <TD>
         31.48 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         30.25 ns
      </TD>
      <TD CLASS = "thinright">
         30.50 ns
      </TD>
      <TD>
         31.28 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         30.38 ns
      </TD>
      <TD CLASS = "thinright">
         30.83 ns
      </TD>
      <TD>
         30.31 ns
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         42.60 ns
      </TD>
      <TD>
         42.41 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         30.12 ns
      </TD>
      <TD>
         30.31 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         30.78 ns
      </TD>
      <TD>
         29.97 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         30.68 ns
      </TD>
      <TD>
         29.80 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 5 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 1.38 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 1.37 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 1.37 times faster than Alga (Algebra.Graph)

## vertexList
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         133.6 ns
      </TD>
      <TD CLASS = "thinright">
         1.920 μs
      </TD>
      <TD>
         47.34 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         58.20 ns
      </TD>
      <TD CLASS = "thinright">
         165.0 ns
      </TD>
      <TD>
         1.090 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         105.2 ns
      </TD>
      <TD CLASS = "thinright">
         498.7 ns
      </TD>
      <TD>
         4.488 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         142.5 ns
      </TD>
      <TD CLASS = "thinright">
         616.5 ns
      </TD>
      <TD>
         5.713 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         69.18 ns
      </TD>
      <TD CLASS = "thinright">
         232.9 ns
      </TD>
      <TD>
         2.164 μs
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         309.7 ns
      </TD>
      <TD>
         13.94 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         56.35 ns
      </TD>
      <TD>
         163.1 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         104.8 ns
      </TD>
      <TD>
         485.3 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         135.7 ns
      </TD>
      <TD>
         596.0 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         67.34 ns
      </TD>
      <TD>
         220.3 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 5 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 19.98 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 7.60 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 9.47 times faster than Alga (Algebra.Graph)
 * Containers (Data.Graph) was 29.66 times faster than Alga (Algebra.Graph)

## vertexCount
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         189.9 ns
      </TD>
      <TD CLASS = "thinright">
         3.330 μs
      </TD>
      <TD>
         79.02 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         41.42 ns
      </TD>
      <TD CLASS = "thinright">
         112.1 ns
      </TD>
      <TD>
         940.2 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         31.13 ns
      </TD>
      <TD CLASS = "thinright">
         31.82 ns
      </TD>
      <TD>
         31.07 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         46.79 ns
      </TD>
      <TD CLASS = "thinright">
         102.4 ns
      </TD>
      <TD>
         765.1 ns
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         508.4 ns
      </TD>
      <TD>
         25.21 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         41.08 ns
      </TD>
      <TD>
         110.2 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         31.46 ns
      </TD>
      <TD>
         30.76 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         46.02 ns
      </TD>
      <TD>
         101.4 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 5 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 79.91 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 697.96 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 71.89 times faster than Alga (Algebra.Graph)

## edgeCount
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         359.7 ns
      </TD>
      <TD CLASS = "thinright">
         10.37 μs
      </TD>
      <TD>
         196.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         83.59 ns
      </TD>
      <TD CLASS = "thinright">
         699.1 ns
      </TD>
      <TD>
         8.848 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         102.9 ns
      </TD>
      <TD CLASS = "thinright">
         580.3 ns
      </TD>
      <TD>
         6.629 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         165.4 ns
      </TD>
      <TD CLASS = "thinright">
         749.8 ns
      </TD>
      <TD>
         8.206 μs
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         1.784 μs
      </TD>
      <TD>
         89.36 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         209.2 ns
      </TD>
      <TD>
         4.748 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         150.7 ns
      </TD>
      <TD>
         2.306 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         191.1 ns
      </TD>
      <TD>
         1.356 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 5 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 23.02 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 20.31 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 13.73 times faster than Alga (Algebra.Graph)

## edgeList
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         366.6 ns
      </TD>
      <TD CLASS = "thinright">
         10.13 μs
      </TD>
      <TD>
         194.9 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         86.07 ns
      </TD>
      <TD CLASS = "thinright">
         423.7 ns
      </TD>
      <TD>
         4.194 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         104.5 ns
      </TD>
      <TD CLASS = "thinright">
         992.4 ns
      </TD>
      <TD>
         12.57 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         127.8 ns
      </TD>
      <TD CLASS = "thinright">
         968.5 ns
      </TD>
      <TD>
         11.65 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         102.2 ns
      </TD>
      <TD CLASS = "thinright">
         800.9 ns
      </TD>
      <TD>
         9.472 μs
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         1.811 μs
      </TD>
      <TD>
         89.64 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         130.2 ns
      </TD>
      <TD>
         1.953 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         295.6 ns
      </TD>
      <TD>
         7.226 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         276.9 ns
      </TD>
      <TD>
         5.642 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         229.5 ns
      </TD>
      <TD>
         4.351 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 5 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 13.06 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 10.50 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 9.55 times faster than Alga (Algebra.Graph)
 * Containers (Data.Graph) was 26.89 times faster than Alga (Algebra.Graph)

## hasEdge
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         1.613 μs
      </TD>
      <TD CLASS = "thinright">
         7.461 μs
      </TD>
      <TD>
         77.56 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         212.0 ns
      </TD>
      <TD CLASS = "thinright">
         443.7 ns
      </TD>
      <TD>
         605.9 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         146.6 ns
      </TD>
      <TD CLASS = "thinright">
         213.7 ns
      </TD>
      <TD>
         285.8 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         151.7 ns
      </TD>
      <TD CLASS = "thinright">
         150.7 ns
      </TD>
      <TD>
         154.5 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 8 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 209.53 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 121.83 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 72.43 times faster than Alga (Algebra.Graph)

## add a new edge
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         88.64 ns
      </TD>
      <TD CLASS = "thinright">
         359.0 ns
      </TD>
      <TD>
         4.248 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         221.2 ns
      </TD>
      <TD CLASS = "thinright">
         994.2 ns
      </TD>
      <TD>
         9.319 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         492.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.203 μs
      </TD>
      <TD>
         6.523 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         400.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.776 μs
      </TD>
      <TD>
         19.48 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga (Algebra.Graph) was the fastest 8 times


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 0.52 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)
 * Fgl (Data.Graph.Inductive.Tree) was 0.99 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)
 * Alga (Algebra.Graph) was 2.46 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)

## add a new vertex
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         65.17 ns
      </TD>
      <TD CLASS = "thinright">
         338.2 ns
      </TD>
      <TD>
         4.400 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         135.7 ns
      </TD>
      <TD CLASS = "thinright">
         870.3 ns
      </TD>
      <TD>
         9.746 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         145.9 ns
      </TD>
      <TD CLASS = "thinright">
         594.1 ns
      </TD>
      <TD>
         5.512 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         262.1 ns
      </TD>
      <TD CLASS = "thinright">
         1.624 μs
      </TD>
      <TD>
         19.03 μs
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         142.2 ns
      </TD>
      <TD>
         2.832 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         254.9 ns
      </TD>
      <TD>
         4.848 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         187.4 ns
      </TD>
      <TD>
         1.770 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         547.7 ns
      </TD>
      <TD>
         8.316 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga (Algebra.Graph) was the fastest 5 times


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 0.52 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)
 * Fgl (Data.Graph.Inductive.Tree) was 1.70 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)
 * Alga (Algebra.Graph) was 2.03 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)

## remove a vertex
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         78.29 ns
      </TD>
      <TD CLASS = "thinright">
         1.060 μs
      </TD>
      <TD>
         14.94 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         148.1 ns
      </TD>
      <TD CLASS = "thinright">
         901.3 ns
      </TD>
      <TD>
         9.431 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         181.8 ns
      </TD>
      <TD CLASS = "thinright">
         640.6 ns
      </TD>
      <TD>
         5.362 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         404.9 ns
      </TD>
      <TD CLASS = "thinright">
         1.916 μs
      </TD>
      <TD>
         18.54 μs
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         384.2 ns
      </TD>
      <TD>
         10.20 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         228.9 ns
      </TD>
      <TD>
         5.323 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         339.2 ns
      </TD>
      <TD>
         7.209 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         898.1 ns
      </TD>
      <TD>
         14.23 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga (Algebra.Graph) was the fastest 1 times
There was 4 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 0.54 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 1.48 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 1.38 times faster than Alga (Algebra.Graph)

## equality
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         998.4 ns
      </TD>
      <TD CLASS = "thinright">
         15.11 μs
      </TD>
      <TD>
         294.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         47.49 ns
      </TD>
      <TD CLASS = "thinright">
         136.3 ns
      </TD>
      <TD>
         1.101 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         661.5 ns
      </TD>
      <TD CLASS = "thinright">
         4.907 μs
      </TD>
      <TD>
         60.17 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         775.0 ns
      </TD>
      <TD CLASS = "thinright">
         6.621 μs
      </TD>
      <TD>
         99.26 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         242.2 ns
      </TD>
      <TD CLASS = "thinright">
         1.606 μs
      </TD>
      <TD>
         20.38 μs
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         3.145 μs
      </TD>
      <TD>
         130.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         55.31 ns
      </TD>
      <TD>
         532.8 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         1.592 μs
      </TD>
      <TD>
         32.94 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         1.818 μs
      </TD>
      <TD>
         45.08 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         473.6 ns
      </TD>
      <TD>
         8.500 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 10 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 208.79 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 15.94 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 19.02 times faster than Alga (Algebra.Graph)
 * Containers (Data.Graph) was 782.59 times faster than Alga (Algebra.Graph)

## remove an edge
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         572.1 ns
      </TD>
      <TD CLASS = "thinright">
         5.105 μs
      </TD>
      <TD>
         67.29 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         352.4 ns
      </TD>
      <TD CLASS = "thinright">
         1.499 μs
      </TD>
      <TD>
         9.989 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         314.9 ns
      </TD>
      <TD CLASS = "thinright">
         1.147 μs
      </TD>
      <TD>
         6.195 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         231.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.568 μs
      </TD>
      <TD>
         18.87 μs
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         2.136 μs
      </TD>
      <TD>
         42.14 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         659.8 ns
      </TD>
      <TD>
         9.727 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         614.1 ns
      </TD>
      <TD>
         9.267 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         501.8 ns
      </TD>
      <TD>
         8.267 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 13 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 3.92 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 5.55 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 4.22 times faster than Alga (Algebra.Graph)

## transpose
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         71.59 ns
      </TD>
      <TD CLASS = "thinright">
         817.4 ns
      </TD>
      <TD>
         11.18 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         173.8 ns
      </TD>
      <TD CLASS = "thinright">
         951.5 ns
      </TD>
      <TD>
         9.500 μs
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Alga (Algebra.Graph)
      </TH>
      <TD CLASS = "thinright">
         263.9 ns
      </TD>
      <TD>
         7.647 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         285.4 ns
      </TD>
      <TD>
         4.958 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga (Algebra.Graph) was the fastest 2 times
There was 3 ex-aequo


ABSTRACT:

 * Alga (Algebra.Graph) was 1.02 times faster than Containers (Data.Graph)

## make a Mesh from a list

SUMMARY:

There was 3 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 0.27 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 0.15 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 0.30 times faster than Alga (Algebra.Graph)
 * Containers (Data.Graph) was 3.38 times faster than Alga (Algebra.Graph)

## make a Complete from a list

SUMMARY:

There was 2 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 0.39 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 0.14 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 0.48 times faster than Alga (Algebra.Graph)
 * Containers (Data.Graph) was 5.83 times faster than Alga (Algebra.Graph)

## dff
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         256.8 ns
      </TD>
      <TD CLASS = "thinright">
         1.495 μs
      </TD>
      <TD>
         14.15 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         565.1 ns
      </TD>
      <TD CLASS = "thinright">
         5.511 μs
      </TD>
      <TD>
         75.02 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         634.0 ns
      </TD>
      <TD CLASS = "thinright">
         5.877 μs
      </TD>
      <TD>
         77.34 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         176.0 ns
      </TD>
      <TD CLASS = "thinright">
         2.318 μs
      </TD>
      <TD>
         48.95 μs
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         372.4 ns
      </TD>
      <TD>
         4.744 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         654.6 ns
      </TD>
      <TD>
         12.72 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         680.6 ns
      </TD>
      <TD>
         33.07 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         504.7 ns
      </TD>
      <TD>
         12.35 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers (Data.Graph) was the fastest 5 times


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 1.89 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)
 * Fgl (Data.Graph.Inductive.Tree) was 0.83 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)
 * Containers (Data.Graph) was 2.69 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)

## topSort
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         278.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.646 μs
      </TD>
      <TD>
         15.12 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         907.6 ns
      </TD>
      <TD CLASS = "thinright">
         8.054 μs
      </TD>
      <TD>
         116.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         1.002 μs
      </TD>
      <TD CLASS = "thinright">
         8.089 μs
      </TD>
      <TD>
         117.1 μs
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         397.5 ns
      </TD>
      <TD>
         4.860 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         1.235 μs
      </TD>
      <TD>
         16.70 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         1.220 μs
      </TD>
      <TD>
         37.62 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers (Data.Graph) was the fastest 5 times


ABSTRACT:

 * Fgl (Data.Graph.Inductive.Tree) was 0.85 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)
 * Containers (Data.Graph) was 3.99 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)

## reachable
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         235.3 ns
      </TD>
      <TD CLASS = "thinright">
         1.050 μs
      </TD>
      <TD>
         10.63 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         711.1 ns
      </TD>
      <TD CLASS = "thinright">
         5.579 μs
      </TD>
      <TD>
         81.01 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         730.7 ns
      </TD>
      <TD CLASS = "thinright">
         5.509 μs
      </TD>
      <TD>
         80.06 μs
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         342.5 ns
      </TD>
      <TD>
         4.396 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         961.8 ns
      </TD>
      <TD>
         15.12 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         973.5 ns
      </TD>
      <TD>
         34.66 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers (Data.Graph) was the fastest 5 times


ABSTRACT:

 * Fgl (Data.Graph.Inductive.Tree) was 0.87 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)
 * Containers (Data.Graph) was 3.85 times faster than Fgl (Data.Graph.Inductive.PatriciaTree)

## merge a context
### Mesh

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         157.5 ns
      </TD>
      <TD CLASS = "thinright">
         966.9 ns
      </TD>
      <TD>
         9.725 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         204.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.548 μs
      </TD>
      <TD>
         18.82 μs
      </TD>
   </TR>
</TABLE>

### Complete

<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH>
         10
      </TH>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         220.3 ns
      </TD>
      <TD>
         4.406 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         343.1 ns
      </TD>
      <TD>
         7.279 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl (Data.Graph.Inductive.PatriciaTree) was the fastest 5 times


ABSTRACT:

 * Fgl (Data.Graph.Inductive.PatriciaTree) was 1.58 times faster than Hash-Graph (Data.HashGraph.Strict)

