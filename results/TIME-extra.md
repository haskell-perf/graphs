The content of this fil was obtained with **modified** sources: `bench/Alga/Graph.hs` was modified, replacing:
```Haskell
instance GraphImpl (Graph Int) where
  mkGraph = mk
```
by

```Haskell
instance GraphImpl (Graph Int) where
  mkGraph = mkSpecClique
```
# Benchmarks

Doing:

----
* [edgeList](#edgelist)
* [vertexList](#vertexlist)
* [equality](#equality)
* [transpose](#transpose)
* [vertexCount](#vertexcount)
* [edgeCount](#edgecount)
* [hasEdge](#hasedge)
* [isEmpty](#isempty)
* [hasVertex](#hasvertex)
* [addEdge](#addedge)
* [addVertex](#addvertex)
* [removeVertex](#removevertex)
* [removeEdge](#removeedge)
----

Using [("Clique",4)] as graphs

## edgeList

Description: Produce a list of the edges in the graph

### Clique
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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         48.07 ns
      </TD>
      <TD CLASS = "thinright">
         6.014 μs
      </TD>
      <TD CLASS = "thinright">
         366.1 μs
      </TD>
      <TD>
         75.24 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         39.07 ns
      </TD>
      <TD CLASS = "thinright">
         954.1 ns
      </TD>
      <TD CLASS = "thinright">
         80.78 μs
      </TD>
      <TD>
         25.39 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         34.22 ns
      </TD>
      <TD CLASS = "thinright">
         3.049 μs
      </TD>
      <TD CLASS = "thinright">
         341.2 μs
      </TD>
      <TD>
         127.7 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         38.09 ns
      </TD>
      <TD CLASS = "thinright">
         1.910 μs
      </TD>
      <TD CLASS = "thinright">
         311.4 μs
      </TD>
      <TD>
         220.5 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 3 times
 * Fgl was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 8.69 times faster than Hash-Graph
 * Alga was 2.93 times faster than Hash-Graph
 * Fgl was 1.73 times faster than Hash-Graph

## vertexList

Description: Produce a list of the vertices in the graph

### Clique
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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         44.34 ns
      </TD>
      <TD CLASS = "thinright">
         1.395 μs
      </TD>
      <TD CLASS = "thinright">
         28.87 μs
      </TD>
      <TD>
         459.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         33.08 ns
      </TD>
      <TD CLASS = "thinright">
         148.9 ns
      </TD>
      <TD CLASS = "thinright">
         1.127 μs
      </TD>
      <TD>
         10.73 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         53.84 ns
      </TD>
      <TD CLASS = "thinright">
         420.9 ns
      </TD>
      <TD CLASS = "thinright">
         4.303 μs
      </TD>
      <TD>
         43.57 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.32 ns
      </TD>
      <TD CLASS = "thinright">
         197.4 ns
      </TD>
      <TD CLASS = "thinright">
         2.258 μs
      </TD>
      <TD>
         25.37 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 3 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 42.85 times faster than Alga
 * Hash-Graph was 18.12 times faster than Alga
 * Fgl was 10.55 times faster than Alga

## equality

Description: Test if two graphs are equals

### Clique
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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         445.7 ns
      </TD>
      <TD CLASS = "thinright">
         9.593 μs
      </TD>
      <TD CLASS = "thinright">
         555.0 μs
      </TD>
      <TD>
         130.7 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         30.52 ns
      </TD>
      <TD CLASS = "thinright">
         270.3 ns
      </TD>
      <TD CLASS = "thinright">
         23.96 μs
      </TD>
      <TD>
         10.98 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         166.2 ns
      </TD>
      <TD CLASS = "thinright">
         13.99 μs
      </TD>
      <TD CLASS = "thinright">
         2.957 ms
      </TD>
      <TD>
         503.1 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         68.30 ns
      </TD>
      <TD CLASS = "thinright">
         4.135 μs
      </TD>
      <TD CLASS = "thinright">
         452.5 μs
      </TD>
      <TD>
         56.81 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 7 times

 There was 1 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 89.26 times faster than Fgl
 * Hash-Graph was 15.71 times faster than Fgl
 * Alga was 5.07 times faster than Fgl

## transpose

Description: Transpose (invert all the edges) the graph

### Clique
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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         27.30 ns
      </TD>
      <TD CLASS = "thinright">
         299.8 ns
      </TD>
      <TD CLASS = "thinright">
         3.016 μs
      </TD>
      <TD>
         31.03 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         89.62 ns
      </TD>
      <TD CLASS = "thinright">
         2.121 μs
      </TD>
      <TD CLASS = "thinright">
         263.7 μs
      </TD>
      <TD>
         79.36 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 4 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 2557.16 times faster than Containers

## vertexCount

Description: Count the vertices of the graph

### Clique
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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         32.15 ns
      </TD>
      <TD CLASS = "thinright">
         1.183 μs
      </TD>
      <TD CLASS = "thinright">
         26.06 μs
      </TD>
      <TD>
         434.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         20.93 ns
      </TD>
      <TD CLASS = "thinright">
         20.52 ns
      </TD>
      <TD CLASS = "thinright">
         22.33 ns
      </TD>
      <TD>
         20.48 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         24.04 ns
      </TD>
      <TD CLASS = "thinright">
         102.5 ns
      </TD>
      <TD CLASS = "thinright">
         942.0 ns
      </TD>
      <TD>
         10.29 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.35 ns
      </TD>
      <TD CLASS = "thinright">
         88.99 ns
      </TD>
      <TD CLASS = "thinright">
         797.1 ns
      </TD>
      <TD>
         9.006 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 4 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 21209.74 times faster than Alga
 * Hash-Graph was 48.22 times faster than Alga
 * Fgl was 42.21 times faster than Alga

## edgeCount

Description: Count the edges of the graph

### Clique
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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         55.18 ns
      </TD>
      <TD CLASS = "thinright">
         6.311 μs
      </TD>
      <TD CLASS = "thinright">
         349.7 μs
      </TD>
      <TD>
         73.80 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         30.79 ns
      </TD>
      <TD CLASS = "thinright">
         262.6 ns
      </TD>
      <TD CLASS = "thinright">
         26.85 μs
      </TD>
      <TD>
         26.26 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         34.88 ns
      </TD>
      <TD CLASS = "thinright">
         1.985 μs
      </TD>
      <TD CLASS = "thinright">
         207.2 μs
      </TD>
      <TD>
         104.9 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         39.75 ns
      </TD>
      <TD CLASS = "thinright">
         997.7 ns
      </TD>
      <TD CLASS = "thinright">
         59.34 μs
      </TD>
      <TD>
         6.379 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 3 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 16.45 times faster than Fgl
 * Containers was 4.00 times faster than Fgl
 * Alga was 1.42 times faster than Fgl

## hasEdge

Description: Test if the given edge is in the graph (with arguments both in the graph and not in the graph (where applicable))

### Clique
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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         679.0 ns
      </TD>
      <TD CLASS = "thinright">
         2.005 μs
      </TD>
      <TD CLASS = "thinright">
         7.297 μs
      </TD>
      <TD>
         59.87 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         28.98 ns
      </TD>
      <TD CLASS = "thinright">
         116.6 ns
      </TD>
      <TD CLASS = "thinright">
         1.143 μs
      </TD>
      <TD>
         10.30 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         115.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.390 μs
      </TD>
      <TD CLASS = "thinright">
         18.92 μs
      </TD>
      <TD>
         491.6 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         106.5 ns
      </TD>
      <TD CLASS = "thinright">
         145.9 ns
      </TD>
      <TD CLASS = "thinright">
         167.3 ns
      </TD>
      <TD>
         177.0 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph was the fastest 13 times
 * Containers was the fastest 6 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 3157.39 times faster than Fgl
 * Containers was 49.29 times faster than Fgl
 * Alga was 8.70 times faster than Fgl

## isEmpty

Description: Test if the graph is empty

### Clique
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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         21.18 ns
      </TD>
      <TD CLASS = "thinright">
         24.69 ns
      </TD>
      <TD CLASS = "thinright">
         25.05 ns
      </TD>
      <TD>
         25.07 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         20.22 ns
      </TD>
      <TD CLASS = "thinright">
         20.21 ns
      </TD>
      <TD CLASS = "thinright">
         20.34 ns
      </TD>
      <TD>
         20.30 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         20.15 ns
      </TD>
      <TD CLASS = "thinright">
         20.10 ns
      </TD>
      <TD CLASS = "thinright">
         20.06 ns
      </TD>
      <TD>
         20.14 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:


 There was 4 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 1.24 times faster than Alga
 * Fgl was 1.23 times faster than Alga

## hasVertex

Description: Test if the given vertex is in the graph

### Clique
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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         33.58 ns
      </TD>
      <TD CLASS = "thinright">
         128.3 ns
      </TD>
      <TD CLASS = "thinright">
         892.2 ns
      </TD>
      <TD>
         8.586 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         53.80 ns
      </TD>
      <TD CLASS = "thinright">
         664.1 ns
      </TD>
      <TD CLASS = "thinright">
         11.58 μs
      </TD>
      <TD>
         327.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.50 ns
      </TD>
      <TD CLASS = "thinright">
         33.67 ns
      </TD>
      <TD CLASS = "thinright">
         37.71 ns
      </TD>
      <TD>
         42.50 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph was the fastest 10 times
 * Alga was the fastest 1 times

 There was 3 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 224.55 times faster than Alga
 * Fgl was 94.75 times faster than Alga

## addEdge

Description: Add an edge (not already in the graph)

### Clique
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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         48.51 ns
      </TD>
      <TD CLASS = "thinright">
         205.6 ns
      </TD>
      <TD CLASS = "thinright">
         1.176 μs
      </TD>
      <TD>
         11.28 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         117.1 ns
      </TD>
      <TD CLASS = "thinright">
         2.083 μs
      </TD>
      <TD CLASS = "thinright">
         192.1 μs
      </TD>
      <TD>
         29.27 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         169.9 ns
      </TD>
      <TD CLASS = "thinright">
         4.242 μs
      </TD>
      <TD CLASS = "thinright">
         385.3 μs
      </TD>
      <TD>
         39.72 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 13 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 3517.50 times faster than Hash-Graph
 * Fgl was 1.36 times faster than Hash-Graph

## addVertex

Description: Add a vertex (not already in the graph)

### Clique
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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         37.30 ns
      </TD>
      <TD CLASS = "thinright">
         146.1 ns
      </TD>
      <TD CLASS = "thinright">
         1.127 μs
      </TD>
      <TD>
         11.00 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         72.45 ns
      </TD>
      <TD CLASS = "thinright">
         1.911 μs
      </TD>
      <TD CLASS = "thinright">
         191.1 μs
      </TD>
      <TD>
         29.54 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         107.2 ns
      </TD>
      <TD CLASS = "thinright">
         3.958 μs
      </TD>
      <TD CLASS = "thinright">
         372.3 μs
      </TD>
      <TD>
         41.24 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 8 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 3739.68 times faster than Hash-Graph
 * Fgl was 1.40 times faster than Hash-Graph

## removeVertex

Description: Remove a vertex of the graph

### Clique
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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         34.79 ns
      </TD>
      <TD CLASS = "thinright">
         350.0 ns
      </TD>
      <TD CLASS = "thinright">
         3.091 μs
      </TD>
      <TD>
         32.64 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         62.98 ns
      </TD>
      <TD CLASS = "thinright">
         2.319 μs
      </TD>
      <TD CLASS = "thinright">
         202.5 μs
      </TD>
      <TD>
         29.18 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         74.08 ns
      </TD>
      <TD CLASS = "thinright">
         7.320 μs
      </TD>
      <TD CLASS = "thinright">
         519.0 μs
      </TD>
      <TD>
         48.43 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 8 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 1482.40 times faster than Hash-Graph
 * Fgl was 1.66 times faster than Hash-Graph

## removeEdge

Description: Remove an edge of the graph

### Clique
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         10
      </TH>
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         2.900 μs
      </TD>
      <TD CLASS = "thinright">
         26.62 μs
      </TD>
      <TD>
         333.6 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         4.471 μs
      </TD>
      <TD CLASS = "thinright">
         233.1 μs
      </TD>
      <TD>
         32.85 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         4.014 μs
      </TD>
      <TD CLASS = "thinright">
         378.2 μs
      </TD>
      <TD>
         40.29 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 9 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 120.87 times faster than Hash-Graph
 * Fgl was 1.23 times faster than Hash-Graph
