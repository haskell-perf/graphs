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
         36.69 ns
      </TD>
      <TD CLASS = "thinright">
         43.41 ns
      </TD>
      <TD>
         43.40 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         30.08 ns
      </TD>
      <TD CLASS = "thinright">
         30.60 ns
      </TD>
      <TD>
         30.33 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         31.56 ns
      </TD>
      <TD CLASS = "thinright">
         30.37 ns
      </TD>
      <TD>
         30.13 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         30.82 ns
      </TD>
      <TD CLASS = "thinright">
         30.48 ns
      </TD>
      <TD>
         31.60 ns
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
         42.79 ns
      </TD>
      <TD>
         43.57 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         30.34 ns
      </TD>
      <TD>
         30.21 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         30.06 ns
      </TD>
      <TD>
         29.88 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         30.20 ns
      </TD>
      <TD>
         30.19 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 5 ex-aequo


ABSTRACT:

 * Fgl (Data.Graph.Inductive.PatriciaTree) was 1.38 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 1.38 times faster than Alga (Algebra.Graph)
 * Hash-Graph (Data.HashGraph.Strict) was 1.37 times faster than Alga (Algebra.Graph)

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
         134.3 ns
      </TD>
      <TD CLASS = "thinright">
         1.833 μs
      </TD>
      <TD>
         46.59 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         56.91 ns
      </TD>
      <TD CLASS = "thinright">
         159.3 ns
      </TD>
      <TD>
         1.078 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         95.20 ns
      </TD>
      <TD CLASS = "thinright">
         482.9 ns
      </TD>
      <TD>
         4.290 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         146.4 ns
      </TD>
      <TD CLASS = "thinright">
         579.3 ns
      </TD>
      <TD>
         5.259 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         67.75 ns
      </TD>
      <TD CLASS = "thinright">
         226.4 ns
      </TD>
      <TD>
         2.225 μs
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
         299.1 ns
      </TD>
      <TD>
         12.99 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         56.75 ns
      </TD>
      <TD>
         154.2 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         94.21 ns
      </TD>
      <TD>
         461.7 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         132.5 ns
      </TD>
      <TD>
         576.6 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         66.35 ns
      </TD>
      <TD>
         220.3 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers (Data.Graph) was the fastest 5 times


ABSTRACT:

 * Containers (Data.Graph) was 29.32 times faster than Alga (Algebra.Graph)
 * Hash-Graph (Data.HashGraph.Strict) was 18.90 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 9.48 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 7.55 times faster than Alga (Algebra.Graph)

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
         177.7 ns
      </TD>
      <TD CLASS = "thinright">
         3.189 μs
      </TD>
      <TD>
         78.70 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         41.66 ns
      </TD>
      <TD CLASS = "thinright">
         110.9 ns
      </TD>
      <TD>
         855.5 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         31.26 ns
      </TD>
      <TD CLASS = "thinright">
         31.20 ns
      </TD>
      <TD>
         31.05 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         47.10 ns
      </TD>
      <TD CLASS = "thinright">
         105.8 ns
      </TD>
      <TD>
         767.5 ns
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
         505.5 ns
      </TD>
      <TD>
         23.57 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         43.40 ns
      </TD>
      <TD>
         109.5 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         30.86 ns
      </TD>
      <TD>
         30.74 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         47.49 ns
      </TD>
      <TD>
         102.7 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl (Data.Graph.Inductive.Tree) was the fastest 5 times


ABSTRACT:

 * Fgl (Data.Graph.Inductive.Tree) was 685.14 times faster than Alga (Algebra.Graph)
 * Hash-Graph (Data.HashGraph.Strict) was 75.32 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 70.40 times faster than Alga (Algebra.Graph)

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
         373.7 ns
      </TD>
      <TD CLASS = "thinright">
         9.677 μs
      </TD>
      <TD>
         194.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         84.10 ns
      </TD>
      <TD CLASS = "thinright">
         693.5 ns
      </TD>
      <TD>
         8.912 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         86.12 ns
      </TD>
      <TD CLASS = "thinright">
         536.3 ns
      </TD>
      <TD>
         5.887 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         173.8 ns
      </TD>
      <TD CLASS = "thinright">
         815.1 ns
      </TD>
      <TD>
         8.116 μs
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
         1.766 μs
      </TD>
      <TD>
         89.85 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         208.7 ns
      </TD>
      <TD>
         4.763 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         151.2 ns
      </TD>
      <TD>
         2.228 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         200.7 ns
      </TD>
      <TD>
         1.381 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl (Data.Graph.Inductive.Tree) was the fastest 3 times
 * Hash-Graph (Data.HashGraph.Strict) was the fastest 1 times
There was 1 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 22.38 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 21.50 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 13.52 times faster than Alga (Algebra.Graph)

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
         367.9 ns
      </TD>
      <TD CLASS = "thinright">
         9.770 μs
      </TD>
      <TD>
         191.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         83.72 ns
      </TD>
      <TD CLASS = "thinright">
         427.1 ns
      </TD>
      <TD>
         4.223 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         101.3 ns
      </TD>
      <TD CLASS = "thinright">
         954.2 ns
      </TD>
      <TD>
         12.24 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         125.9 ns
      </TD>
      <TD CLASS = "thinright">
         879.6 ns
      </TD>
      <TD>
         10.74 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         101.7 ns
      </TD>
      <TD CLASS = "thinright">
         735.8 ns
      </TD>
      <TD>
         9.534 μs
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
         1.794 μs
      </TD>
      <TD>
         88.44 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         135.4 ns
      </TD>
      <TD>
         2.037 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         289.0 ns
      </TD>
      <TD>
         7.186 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         274.9 ns
      </TD>
      <TD>
         5.280 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         224.8 ns
      </TD>
      <TD>
         4.370 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers (Data.Graph) was the fastest 5 times


ABSTRACT:

 * Containers (Data.Graph) was 25.88 times faster than Alga (Algebra.Graph)
 * Hash-Graph (Data.HashGraph.Strict) was 13.05 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 11.03 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 9.61 times faster than Alga (Algebra.Graph)

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
         1.618 μs
      </TD>
      <TD CLASS = "thinright">
         7.540 μs
      </TD>
      <TD>
         79.07 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         243.5 ns
      </TD>
      <TD CLASS = "thinright">
         464.1 ns
      </TD>
      <TD>
         652.0 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         149.0 ns
      </TD>
      <TD CLASS = "thinright">
         211.4 ns
      </TD>
      <TD>
         291.9 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         150.9 ns
      </TD>
      <TD CLASS = "thinright">
         150.3 ns
      </TD>
      <TD>
         154.1 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph (Data.HashGraph.Strict) was the fastest 5 times
 * Fgl (Data.Graph.Inductive.Tree) was the fastest 1 times
There was 2 ex-aequo


ABSTRACT:

 * Hash-Graph (Data.HashGraph.Strict) was 213.84 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 121.92 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 66.23 times faster than Alga (Algebra.Graph)

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
         89.99 ns
      </TD>
      <TD CLASS = "thinright">
         417.3 ns
      </TD>
      <TD>
         5.012 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         219.6 ns
      </TD>
      <TD CLASS = "thinright">
         996.6 ns
      </TD>
      <TD>
         9.322 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         530.1 ns
      </TD>
      <TD CLASS = "thinright">
         1.279 μs
      </TD>
      <TD>
         6.614 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         410.2 ns
      </TD>
      <TD CLASS = "thinright">
         1.687 μs
      </TD>
      <TD>
         19.29 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga (Algebra.Graph) was the fastest 8 times


ABSTRACT:

 * Alga (Algebra.Graph) was 4.08 times faster than Hash-Graph (Data.HashGraph.Strict)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 1.88 times faster than Hash-Graph (Data.HashGraph.Strict)
 * Fgl (Data.Graph.Inductive.Tree) was 1.85 times faster than Hash-Graph (Data.HashGraph.Strict)

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
         57.02 ns
      </TD>
      <TD CLASS = "thinright">
         405.1 ns
      </TD>
      <TD>
         4.905 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         131.5 ns
      </TD>
      <TD CLASS = "thinright">
         848.5 ns
      </TD>
      <TD>
         9.305 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         162.2 ns
      </TD>
      <TD CLASS = "thinright">
         636.9 ns
      </TD>
      <TD>
         5.452 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         280.9 ns
      </TD>
      <TD CLASS = "thinright">
         1.470 μs
      </TD>
      <TD>
         19.02 μs
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
         154.7 ns
      </TD>
      <TD>
         3.227 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         252.7 ns
      </TD>
      <TD>
         4.941 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         214.0 ns
      </TD>
      <TD>
         1.880 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         503.7 ns
      </TD>
      <TD>
         7.581 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga (Algebra.Graph) was the fastest 4 times
 * Fgl (Data.Graph.Inductive.Tree) was the fastest 1 times


ABSTRACT:

 * Alga (Algebra.Graph) was 3.40 times faster than Hash-Graph (Data.HashGraph.Strict)
 * Fgl (Data.Graph.Inductive.Tree) was 2.95 times faster than Hash-Graph (Data.HashGraph.Strict)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 1.84 times faster than Hash-Graph (Data.HashGraph.Strict)

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
         76.08 ns
      </TD>
      <TD CLASS = "thinright">
         1.075 μs
      </TD>
      <TD>
         14.76 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         147.3 ns
      </TD>
      <TD CLASS = "thinright">
         872.5 ns
      </TD>
      <TD>
         9.260 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         185.2 ns
      </TD>
      <TD CLASS = "thinright">
         633.5 ns
      </TD>
      <TD>
         5.460 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         391.5 ns
      </TD>
      <TD CLASS = "thinright">
         1.870 μs
      </TD>
      <TD>
         19.05 μs
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
         367.7 ns
      </TD>
      <TD>
         10.06 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         224.0 ns
      </TD>
      <TD>
         5.317 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         346.7 ns
      </TD>
      <TD>
         7.030 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         877.5 ns
      </TD>
      <TD>
         13.70 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl (Data.Graph.Inductive.PatriciaTree) was the fastest 2 times
 * Fgl (Data.Graph.Inductive.Tree) was the fastest 2 times
 * Alga (Algebra.Graph) was the fastest 1 times


ABSTRACT:

 * Fgl (Data.Graph.Inductive.Tree) was 2.71 times faster than Hash-Graph (Data.HashGraph.Strict)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 2.55 times faster than Hash-Graph (Data.HashGraph.Strict)
 * Alga (Algebra.Graph) was 1.85 times faster than Hash-Graph (Data.HashGraph.Strict)

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
         966.8 ns
      </TD>
      <TD CLASS = "thinright">
         15.05 μs
      </TD>
      <TD>
         290.7 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         46.20 ns
      </TD>
      <TD CLASS = "thinright">
         135.0 ns
      </TD>
      <TD>
         1.095 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         638.9 ns
      </TD>
      <TD CLASS = "thinright">
         4.816 μs
      </TD>
      <TD>
         58.92 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         756.4 ns
      </TD>
      <TD CLASS = "thinright">
         6.636 μs
      </TD>
      <TD>
         98.99 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         238.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.629 μs
      </TD>
      <TD>
         19.81 μs
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
         3.087 μs
      </TD>
      <TD>
         131.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         54.71 ns
      </TD>
      <TD>
         548.3 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         1.483 μs
      </TD>
      <TD>
         31.87 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         1.781 μs
      </TD>
      <TD>
         41.72 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         470.9 ns
      </TD>
      <TD>
         7.945 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers (Data.Graph) was the fastest 10 times


ABSTRACT:

 * Containers (Data.Graph) was 875.06 times faster than Alga (Algebra.Graph)
 * Hash-Graph (Data.HashGraph.Strict) was 215.82 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 21.11 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.Tree) was 16.56 times faster than Alga (Algebra.Graph)

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
         563.1 ns
      </TD>
      <TD CLASS = "thinright">
         4.938 μs
      </TD>
      <TD>
         67.24 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         346.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.477 μs
      </TD>
      <TD>
         10.01 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         345.3 ns
      </TD>
      <TD CLASS = "thinright">
         1.202 μs
      </TD>
      <TD>
         6.342 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         220.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.566 μs
      </TD>
      <TD>
         18.97 μs
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
         2.131 μs
      </TD>
      <TD>
         40.95 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         655.6 ns
      </TD>
      <TD>
         9.833 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         651.6 ns
      </TD>
      <TD>
         9.052 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         492.0 ns
      </TD>
      <TD>
         7.273 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph (Data.HashGraph.Strict) was the fastest 7 times
 * Fgl (Data.Graph.Inductive.Tree) was the fastest 6 times


ABSTRACT:

 * Fgl (Data.Graph.Inductive.Tree) was 5.33 times faster than Alga (Algebra.Graph)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 4.16 times faster than Alga (Algebra.Graph)
 * Hash-Graph (Data.HashGraph.Strict) was 4.04 times faster than Alga (Algebra.Graph)

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
         69.64 ns
      </TD>
      <TD CLASS = "thinright">
         793.7 ns
      </TD>
      <TD>
         11.22 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         174.5 ns
      </TD>
      <TD CLASS = "thinright">
         943.2 ns
      </TD>
      <TD>
         9.410 μs
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
         261.9 ns
      </TD>
      <TD>
         7.377 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers (Data.Graph)
      </TH>
      <TD CLASS = "thinright">
         278.6 ns
      </TD>
      <TD>
         4.869 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga (Algebra.Graph) was the fastest 2 times
 * Containers (Data.Graph) was the fastest 2 times
There was 1 ex-aequo


ABSTRACT:

 * Alga (Algebra.Graph) was 1.02 times faster than Containers (Data.Graph)

## make a Mesh from a list

SUMMARY:

 * Containers (Data.Graph) was the fastest 3 times


ABSTRACT:

 * Containers (Data.Graph) was 21.86 times faster than Fgl (Data.Graph.Inductive.Tree)
 * Alga (Algebra.Graph) was 6.58 times faster than Fgl (Data.Graph.Inductive.Tree)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 1.95 times faster than Fgl (Data.Graph.Inductive.Tree)
 * Hash-Graph (Data.HashGraph.Strict) was 1.81 times faster than Fgl (Data.Graph.Inductive.Tree)

## make a Complete from a list

SUMMARY:

 * Containers (Data.Graph) was the fastest 2 times


ABSTRACT:

 * Containers (Data.Graph) was 41.24 times faster than Fgl (Data.Graph.Inductive.Tree)
 * Alga (Algebra.Graph) was 7.37 times faster than Fgl (Data.Graph.Inductive.Tree)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 3.41 times faster than Fgl (Data.Graph.Inductive.Tree)
 * Hash-Graph (Data.HashGraph.Strict) was 2.88 times faster than Fgl (Data.Graph.Inductive.Tree)

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
         257.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.551 μs
      </TD>
      <TD>
         14.86 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         547.5 ns
      </TD>
      <TD CLASS = "thinright">
         5.417 μs
      </TD>
      <TD>
         75.60 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         684.8 ns
      </TD>
      <TD CLASS = "thinright">
         6.196 μs
      </TD>
      <TD>
         77.94 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         174.1 ns
      </TD>
      <TD CLASS = "thinright">
         2.352 μs
      </TD>
      <TD>
         48.89 μs
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
         368.2 ns
      </TD>
      <TD>
         4.768 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         625.8 ns
      </TD>
      <TD>
         12.61 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         719.4 ns
      </TD>
      <TD>
         32.65 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         423.2 ns
      </TD>
      <TD>
         12.15 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers (Data.Graph) was the fastest 4 times
 * Hash-Graph (Data.HashGraph.Strict) was the fastest 1 times


ABSTRACT:

 * Containers (Data.Graph) was 3.39 times faster than Fgl (Data.Graph.Inductive.Tree)
 * Hash-Graph (Data.HashGraph.Strict) was 2.51 times faster than Fgl (Data.Graph.Inductive.Tree)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 1.30 times faster than Fgl (Data.Graph.Inductive.Tree)

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
         289.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.750 μs
      </TD>
      <TD>
         15.35 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         921.5 ns
      </TD>
      <TD CLASS = "thinright">
         8.139 μs
      </TD>
      <TD>
         120.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         997.7 ns
      </TD>
      <TD CLASS = "thinright">
         8.234 μs
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
         387.5 ns
      </TD>
      <TD>
         5.076 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         1.229 μs
      </TD>
      <TD>
         16.29 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         1.200 μs
      </TD>
      <TD>
         36.65 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers (Data.Graph) was the fastest 5 times


ABSTRACT:

 * Containers (Data.Graph) was 4.56 times faster than Fgl (Data.Graph.Inductive.Tree)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 1.17 times faster than Fgl (Data.Graph.Inductive.Tree)

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
         242.1 ns
      </TD>
      <TD CLASS = "thinright">
         1.003 μs
      </TD>
      <TD>
         10.47 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         709.3 ns
      </TD>
      <TD CLASS = "thinright">
         5.261 μs
      </TD>
      <TD>
         81.12 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         692.4 ns
      </TD>
      <TD CLASS = "thinright">
         5.536 μs
      </TD>
      <TD>
         80.18 μs
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
         343.4 ns
      </TD>
      <TD>
         4.391 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         928.1 ns
      </TD>
      <TD>
         15.24 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl (Data.Graph.Inductive.Tree)
      </TH>
      <TD CLASS = "thinright">
         997.4 ns
      </TD>
      <TD>
         34.41 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers (Data.Graph) was the fastest 5 times


ABSTRACT:

 * Containers (Data.Graph) was 4.41 times faster than Fgl (Data.Graph.Inductive.Tree)
 * Fgl (Data.Graph.Inductive.PatriciaTree) was 1.16 times faster than Fgl (Data.Graph.Inductive.Tree)

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
         147.6 ns
      </TD>
      <TD CLASS = "thinright">
         910.0 ns
      </TD>
      <TD>
         9.259 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         201.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.461 μs
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
         Fgl (Data.Graph.Inductive.PatriciaTree)
      </TH>
      <TD CLASS = "thinright">
         213.9 ns
      </TD>
      <TD>
         4.552 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph (Data.HashGraph.Strict)
      </TH>
      <TD CLASS = "thinright">
         331.6 ns
      </TD>
      <TD>
         6.753 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl (Data.Graph.Inductive.PatriciaTree) was the fastest 5 times


ABSTRACT:

 * Fgl (Data.Graph.Inductive.PatriciaTree) was 1.58 times faster than Hash-Graph (Data.HashGraph.Strict)

