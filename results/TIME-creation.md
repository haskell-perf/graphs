The content of this file was obtained with:
```Bash
$ time run -g '("Clique",4)' -g '("Mesh",4)' -g '("RealLife",4)' -d Html -b
```

# Benchmarks

Doing:

----
* [edgeList](#edgelist)
* [vertexList](#vertexlist)
* [equality](#equality)
* [transpose](#transpose)
* [dff](#dff)
* [topSort](#topsort)
* [reachable](#reachable)
* [vertexCount](#vertexcount)
* [edgeCount](#edgecount)
* [hasEdge](#hasedge)
* [isEmpty](#isempty)
* [hasVertex](#hasvertex)
* [addEdge](#addedge)
* [addVertex](#addvertex)
* [removeVertex](#removevertex)
* [removeEdge](#removeedge)
* [mergeContext](#mergecontext)

----

Using [("Clique",4),("Mesh",4),("RealLife",4)] as graphs

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
         51.60 ns
      </TD>
      <TD CLASS = "thinright">
         41.14 μs
      </TD>
      <TD CLASS = "thinright">
         9.709 ms
      </TD>
      <TD>
         1.388 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         41.48 ns
      </TD>
      <TD CLASS = "thinright">
         1.713 μs
      </TD>
      <TD CLASS = "thinright">
         204.1 μs
      </TD>
      <TD>
         72.94 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         37.03 ns
      </TD>
      <TD CLASS = "thinright">
         18.57 μs
      </TD>
      <TD CLASS = "thinright">
         9.818 ms
      </TD>
      <TD>
         3.312 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         41.11 ns
      </TD>
      <TD CLASS = "thinright">
         20.27 μs
      </TD>
      <TD CLASS = "thinright">
         8.035 ms
      </TD>
      <TD>
         3.827 s
      </TD>
   </TR>
</TABLE>

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
         51.08 ns
      </TD>
      <TD CLASS = "thinright">
         10.95 μs
      </TD>
      <TD CLASS = "thinright">
         211.8 μs
      </TD>
      <TD>
         3.716 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         41.59 ns
      </TD>
      <TD CLASS = "thinright">
         665.7 ns
      </TD>
      <TD CLASS = "thinright">
         8.144 μs
      </TD>
      <TD>
         88.36 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         37.49 ns
      </TD>
      <TD CLASS = "thinright">
         6.122 μs
      </TD>
      <TD CLASS = "thinright">
         216.5 μs
      </TD>
      <TD>
         19.21 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         41.34 ns
      </TD>
      <TD CLASS = "thinright">
         5.598 μs
      </TD>
      <TD CLASS = "thinright">
         199.0 μs
      </TD>
      <TD>
         20.20 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         93.04 ms
      </TD>
      <TD CLASS = "thinright">
         19.40 μs
      </TD>
      <TD CLASS = "thinright">
         571.0 μs
      </TD>
      <TD>
         7.305 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         3.267 ms
      </TD>
      <TD CLASS = "thinright">
         1.051 μs
      </TD>
      <TD CLASS = "thinright">
         17.61 μs
      </TD>
      <TD>
         158.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         559.5 ms
      </TD>
      <TD CLASS = "thinright">
         10.87 μs
      </TD>
      <TD CLASS = "thinright">
         560.6 μs
      </TD>
      <TD>
         20.23 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         604.7 ms
      </TD>
      <TD CLASS = "thinright">
         10.88 μs
      </TD>
      <TD CLASS = "thinright">
         562.5 μs
      </TD>
      <TD>
         19.54 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Fgl was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 95.13 times faster than Hash-Graph
 * Alga was 3.11 times faster than Hash-Graph
 * Fgl was 1.09 times faster than Hash-Graph

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
         52.32 ns
      </TD>
      <TD CLASS = "thinright">
         21.48 μs
      </TD>
      <TD CLASS = "thinright">
         5.260 ms
      </TD>
      <TD>
         820.0 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         35.66 ns
      </TD>
      <TD CLASS = "thinright">
         964.3 ns
      </TD>
      <TD CLASS = "thinright">
         89.64 μs
      </TD>
      <TD>
         72.11 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         56.43 ns
      </TD>
      <TD CLASS = "thinright">
         14.91 μs
      </TD>
      <TD CLASS = "thinright">
         7.976 ms
      </TD>
      <TD>
         3.180 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         27.71 ns
      </TD>
      <TD CLASS = "thinright">
         19.86 μs
      </TD>
      <TD CLASS = "thinright">
         8.758 ms
      </TD>
      <TD>
         3.648 s
      </TD>
   </TR>
</TABLE>

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
         49.69 ns
      </TD>
      <TD CLASS = "thinright">
         5.692 μs
      </TD>
      <TD CLASS = "thinright">
         141.6 μs
      </TD>
      <TD>
         2.587 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         36.02 ns
      </TD>
      <TD CLASS = "thinright">
         417.9 ns
      </TD>
      <TD CLASS = "thinright">
         5.198 μs
      </TD>
      <TD>
         45.91 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         57.35 ns
      </TD>
      <TD CLASS = "thinright">
         5.610 μs
      </TD>
      <TD CLASS = "thinright">
         211.7 μs
      </TD>
      <TD>
         18.19 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         27.64 ns
      </TD>
      <TD CLASS = "thinright">
         5.038 μs
      </TD>
      <TD CLASS = "thinright">
         193.4 μs
      </TD>
      <TD>
         21.48 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         40.62 ms
      </TD>
      <TD CLASS = "thinright">
         10.31 μs
      </TD>
      <TD CLASS = "thinright">
         302.4 μs
      </TD>
      <TD>
         3.742 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         924.8 μs
      </TD>
      <TD CLASS = "thinright">
         572.3 ns
      </TD>
      <TD CLASS = "thinright">
         10.40 μs
      </TD>
      <TD>
         87.63 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         538.1 ms
      </TD>
      <TD CLASS = "thinright">
         9.793 μs
      </TD>
      <TD CLASS = "thinright">
         528.0 μs
      </TD>
      <TD>
         19.23 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         557.8 ms
      </TD>
      <TD CLASS = "thinright">
         9.535 μs
      </TD>
      <TD CLASS = "thinright">
         541.4 μs
      </TD>
      <TD>
         20.19 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 114.32 times faster than Hash-Graph
 * Alga was 4.92 times faster than Hash-Graph
 * Fgl was 1.13 times faster than Hash-Graph

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
         200.6 ns
      </TD>
      <TD CLASS = "thinright">
         61.99 μs
      </TD>
      <TD CLASS = "thinright">
         13.89 ms
      </TD>
      <TD>
         2.053 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         35.63 ns
      </TD>
      <TD CLASS = "thinright">
         1.135 μs
      </TD>
      <TD CLASS = "thinright">
         124.2 μs
      </TD>
      <TD>
         89.06 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         170.2 ns
      </TD>
      <TD CLASS = "thinright">
         30.21 μs
      </TD>
      <TD CLASS = "thinright">
         10.83 ms
      </TD>
      <TD>
         3.813 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         82.88 ns
      </TD>
      <TD CLASS = "thinright">
         23.60 μs
      </TD>
      <TD CLASS = "thinright">
         8.433 ms
      </TD>
      <TD>
         3.757 s
      </TD>
   </TR>
</TABLE>

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
         203.6 ns
      </TD>
      <TD CLASS = "thinright">
         17.23 μs
      </TD>
      <TD CLASS = "thinright">
         329.3 μs
      </TD>
      <TD>
         6.713 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         34.76 ns
      </TD>
      <TD CLASS = "thinright">
         384.1 ns
      </TD>
      <TD CLASS = "thinright">
         5.342 μs
      </TD>
      <TD>
         46.50 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         168.0 ns
      </TD>
      <TD CLASS = "thinright">
         10.77 μs
      </TD>
      <TD CLASS = "thinright">
         272.0 μs
      </TD>
      <TD>
         19.39 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         75.86 ns
      </TD>
      <TD CLASS = "thinright">
         6.810 μs
      </TD>
      <TD CLASS = "thinright">
         225.4 μs
      </TD>
      <TD>
         18.99 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         142.8 ms
      </TD>
      <TD CLASS = "thinright">
         29.12 μs
      </TD>
      <TD CLASS = "thinright">
         878.3 μs
      </TD>
      <TD>
         11.99 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         1.147 ms
      </TD>
      <TD CLASS = "thinright">
         559.3 ns
      </TD>
      <TD CLASS = "thinright">
         11.16 μs
      </TD>
      <TD>
         109.9 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         570.1 ms
      </TD>
      <TD CLASS = "thinright">
         17.83 μs
      </TD>
      <TD CLASS = "thinright">
         668.3 μs
      </TD>
      <TD>
         22.88 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         557.1 ms
      </TD>
      <TD CLASS = "thinright">
         12.27 μs
      </TD>
      <TD CLASS = "thinright">
         585.6 μs
      </TD>
      <TD>
         20.10 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 22 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 96.79 times faster than Fgl
 * Alga was 1.99 times faster than Fgl
 * Hash-Graph was 1.02 times faster than Fgl

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
         32.30 ns
      </TD>
      <TD CLASS = "thinright">
         6.038 μs
      </TD>
      <TD CLASS = "thinright">
         631.4 μs
      </TD>
      <TD>
         61.20 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         102.2 ns
      </TD>
      <TD CLASS = "thinright">
         3.074 μs
      </TD>
      <TD CLASS = "thinright">
         519.5 μs
      </TD>
      <TD>
         163.3 ms
      </TD>
   </TR>
</TABLE>

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
         31.38 ns
      </TD>
      <TD CLASS = "thinright">
         1.680 μs
      </TD>
      <TD CLASS = "thinright">
         22.31 μs
      </TD>
      <TD>
         252.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         99.10 ns
      </TD>
      <TD CLASS = "thinright">
         1.177 μs
      </TD>
      <TD CLASS = "thinright">
         14.01 μs
      </TD>
      <TD>
         201.6 μs
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         3.393 ms
      </TD>
      <TD CLASS = "thinright">
         2.663 μs
      </TD>
      <TD CLASS = "thinright">
         51.84 μs
      </TD>
      <TD>
         405.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         6.443 ms
      </TD>
      <TD CLASS = "thinright">
         1.761 μs
      </TD>
      <TD CLASS = "thinright">
         32.72 μs
      </TD>
      <TD>
         334.3 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 8 times
 * Alga was the fastest 4 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 1.43 times faster than Containers

## dff

Description: Produce a forest, obtainened from a DFS (Deep First Search) of each vertex

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
         Containers
      </TH>
      <TD CLASS = "thinright">
         134.0 ns
      </TD>
      <TD CLASS = "thinright">
         3.568 μs
      </TD>
      <TD CLASS = "thinright">
         343.2 μs
      </TD>
      <TD>
         93.16 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         224.0 ns
      </TD>
      <TD CLASS = "thinright">
         25.24 μs
      </TD>
      <TD CLASS = "thinright">
         15.12 ms
      </TD>
      <TD>
         3.972 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         69.31 ns
      </TD>
      <TD CLASS = "thinright">
         26.56 μs
      </TD>
      <TD CLASS = "thinright">
         7.284 ms
      </TD>
      <TD>
         4.056 s
      </TD>
   </TR>
</TABLE>

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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         137.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.794 μs
      </TD>
      <TD CLASS = "thinright">
         20.14 μs
      </TD>
      <TD>
         276.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         237.1 ns
      </TD>
      <TD CLASS = "thinright">
         12.78 μs
      </TD>
      <TD CLASS = "thinright">
         328.3 μs
      </TD>
      <TD>
         19.71 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         74.47 ns
      </TD>
      <TD CLASS = "thinright">
         8.444 μs
      </TD>
      <TD CLASS = "thinright">
         246.2 μs
      </TD>
      <TD>
         20.18 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         7.959 ms
      </TD>
      <TD CLASS = "thinright">
         2.608 μs
      </TD>
      <TD CLASS = "thinright">
         36.54 μs
      </TD>
      <TD>
         301.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         578.2 ms
      </TD>
      <TD CLASS = "thinright">
         20.19 μs
      </TD>
      <TD CLASS = "thinright">
         794.8 μs
      </TD>
      <TD>
         23.64 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         553.9 ms
      </TD>
      <TD CLASS = "thinright">
         15.20 μs
      </TD>
      <TD CLASS = "thinright">
         644.8 μs
      </TD>
      <TD>
         18.63 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 59.75 times faster than Fgl
 * Hash-Graph was 1.05 times faster than Fgl

## topSort

Description: Topological sorting of the vertices

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
         Containers
      </TH>
      <TD CLASS = "thinright">
         159.7 ns
      </TD>
      <TD CLASS = "thinright">
         3.601 μs
      </TD>
      <TD CLASS = "thinright">
         405.2 μs
      </TD>
      <TD>
         139.7 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         388.4 ns
      </TD>
      <TD CLASS = "thinright">
         30.04 μs
      </TD>
      <TD CLASS = "thinright">
         12.53 ms
      </TD>
      <TD>
         4.131 s
      </TD>
   </TR>
</TABLE>

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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         150.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.833 μs
      </TD>
      <TD CLASS = "thinright">
         20.70 μs
      </TD>
      <TD>
         272.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         363.1 ns
      </TD>
      <TD CLASS = "thinright">
         14.29 μs
      </TD>
      <TD CLASS = "thinright">
         339.8 μs
      </TD>
      <TD>
         22.81 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         7.265 ms
      </TD>
      <TD CLASS = "thinright">
         2.900 μs
      </TD>
      <TD CLASS = "thinright">
         33.89 μs
      </TD>
      <TD>
         307.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         584.3 ms
      </TD>
      <TD CLASS = "thinright">
         25.00 μs
      </TD>
      <TD CLASS = "thinright">
         818.6 μs
      </TD>
      <TD>
         22.95 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 12 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 50.73 times faster than Fgl

## reachable

Description: Produce a list of reachable vertices from a given one

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
         Containers
      </TH>
      <TD CLASS = "thinright">
         133.4 ns
      </TD>
      <TD CLASS = "thinright">
         2.017 μs
      </TD>
      <TD CLASS = "thinright">
         219.1 μs
      </TD>
      <TD>
         83.72 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         255.3 ns
      </TD>
      <TD CLASS = "thinright">
         22.24 μs
      </TD>
      <TD CLASS = "thinright">
         9.167 ms
      </TD>
      <TD>
         3.489 s
      </TD>
   </TR>
</TABLE>

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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         133.7 ns
      </TD>
      <TD CLASS = "thinright">
         886.6 ns
      </TD>
      <TD CLASS = "thinright">
         9.531 μs
      </TD>
      <TD>
         114.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         256.8 ns
      </TD>
      <TD CLASS = "thinright">
         9.233 μs
      </TD>
      <TD CLASS = "thinright">
         246.2 μs
      </TD>
      <TD>
         19.87 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         2.977 ms
      </TD>
      <TD CLASS = "thinright">
         1.360 μs
      </TD>
      <TD CLASS = "thinright">
         27.94 μs
      </TD>
      <TD>
         171.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         579.1 ms
      </TD>
      <TD CLASS = "thinright">
         15.96 μs
      </TD>
      <TD CLASS = "thinright">
         740.0 μs
      </TD>
      <TD>
         20.92 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 22 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 80.93 times faster than Fgl

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
         34.47 ns
      </TD>
      <TD CLASS = "thinright">
         20.16 μs
      </TD>
      <TD CLASS = "thinright">
         4.969 ms
      </TD>
      <TD>
         782.3 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         24.00 ns
      </TD>
      <TD CLASS = "thinright">
         812.9 ns
      </TD>
      <TD CLASS = "thinright">
         93.83 μs
      </TD>
      <TD>
         61.00 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         27.44 ns
      </TD>
      <TD CLASS = "thinright">
         13.46 μs
      </TD>
      <TD CLASS = "thinright">
         5.473 ms
      </TD>
      <TD>
         2.782 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         28.01 ns
      </TD>
      <TD CLASS = "thinright">
         20.05 μs
      </TD>
      <TD CLASS = "thinright">
         7.549 ms
      </TD>
      <TD>
         3.870 s
      </TD>
   </TR>
</TABLE>

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
         35.54 ns
      </TD>
      <TD CLASS = "thinright">
         5.804 μs
      </TD>
      <TD CLASS = "thinright">
         138.7 μs
      </TD>
      <TD>
         2.760 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         24.90 ns
      </TD>
      <TD CLASS = "thinright">
         286.5 ns
      </TD>
      <TD CLASS = "thinright">
         3.259 μs
      </TD>
      <TD>
         33.99 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         27.98 ns
      </TD>
      <TD CLASS = "thinright">
         4.679 μs
      </TD>
      <TD CLASS = "thinright">
         192.6 μs
      </TD>
      <TD>
         19.09 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         30.61 ns
      </TD>
      <TD CLASS = "thinright">
         4.945 μs
      </TD>
      <TD CLASS = "thinright">
         191.6 μs
      </TD>
      <TD>
         20.01 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         40.44 ms
      </TD>
      <TD CLASS = "thinright">
         10.45 μs
      </TD>
      <TD CLASS = "thinright">
         318.6 μs
      </TD>
      <TD>
         3.826 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         965.7 μs
      </TD>
      <TD CLASS = "thinright">
         389.0 ns
      </TD>
      <TD CLASS = "thinright">
         8.657 μs
      </TD>
      <TD>
         80.75 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         530.7 ms
      </TD>
      <TD CLASS = "thinright">
         8.633 μs
      </TD>
      <TD CLASS = "thinright">
         481.3 μs
      </TD>
      <TD>
         19.32 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         581.4 ms
      </TD>
      <TD CLASS = "thinright">
         9.474 μs
      </TD>
      <TD CLASS = "thinright">
         539.3 μs
      </TD>
      <TD>
         19.22 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 12 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 138.48 times faster than Hash-Graph
 * Alga was 5.14 times faster than Hash-Graph
 * Fgl was 1.29 times faster than Hash-Graph

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
         61.43 ns
      </TD>
      <TD CLASS = "thinright">
         43.38 μs
      </TD>
      <TD CLASS = "thinright">
         9.428 ms
      </TD>
      <TD>
         1.284 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         33.62 ns
      </TD>
      <TD CLASS = "thinright">
         1.117 μs
      </TD>
      <TD CLASS = "thinright">
         123.9 μs
      </TD>
      <TD>
         72.52 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         38.94 ns
      </TD>
      <TD CLASS = "thinright">
         18.46 μs
      </TD>
      <TD CLASS = "thinright">
         9.808 ms
      </TD>
      <TD>
         3.402 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         42.19 ns
      </TD>
      <TD CLASS = "thinright">
         19.55 μs
      </TD>
      <TD CLASS = "thinright">
         7.599 ms
      </TD>
      <TD>
         3.428 s
      </TD>
   </TR>
</TABLE>

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
         59.41 ns
      </TD>
      <TD CLASS = "thinright">
         10.83 μs
      </TD>
      <TD CLASS = "thinright">
         215.5 μs
      </TD>
      <TD>
         3.763 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         34.98 ns
      </TD>
      <TD CLASS = "thinright">
         386.6 ns
      </TD>
      <TD CLASS = "thinright">
         4.452 μs
      </TD>
      <TD>
         58.84 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         40.54 ns
      </TD>
      <TD CLASS = "thinright">
         5.957 μs
      </TD>
      <TD CLASS = "thinright">
         233.2 μs
      </TD>
      <TD>
         19.57 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         42.06 ns
      </TD>
      <TD CLASS = "thinright">
         5.845 μs
      </TD>
      <TD CLASS = "thinright">
         208.4 μs
      </TD>
      <TD>
         18.90 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         108.5 ms
      </TD>
      <TD CLASS = "thinright">
         19.24 μs
      </TD>
      <TD CLASS = "thinright">
         592.1 μs
      </TD>
      <TD>
         7.628 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         1.285 ms
      </TD>
      <TD CLASS = "thinright">
         617.6 ns
      </TD>
      <TD CLASS = "thinright">
         12.55 μs
      </TD>
      <TD>
         108.6 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         575.5 ms
      </TD>
      <TD CLASS = "thinright">
         10.43 μs
      </TD>
      <TD CLASS = "thinright">
         552.5 μs
      </TD>
      <TD>
         22.69 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         560.5 ms
      </TD>
      <TD CLASS = "thinright">
         10.74 μs
      </TD>
      <TD CLASS = "thinright">
         557.8 μs
      </TD>
      <TD>
         21.55 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 12 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 103.06 times faster than Fgl
 * Alga was 2.97 times faster than Fgl
 * Hash-Graph was 1.01 times faster than Fgl

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
         768.6 ns
      </TD>
      <TD CLASS = "thinright">
         13.35 μs
      </TD>
      <TD CLASS = "thinright">
         1.147 ms
      </TD>
      <TD>
         290.3 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         35.21 ns
      </TD>
      <TD CLASS = "thinright">
         958.6 ns
      </TD>
      <TD CLASS = "thinright">
         98.26 μs
      </TD>
      <TD>
         71.01 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         124.2 ns
      </TD>
      <TD CLASS = "thinright">
         18.76 μs
      </TD>
      <TD CLASS = "thinright">
         10.41 ms
      </TD>
      <TD>
         3.513 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         128.0 ns
      </TD>
      <TD CLASS = "thinright">
         19.01 μs
      </TD>
      <TD CLASS = "thinright">
         8.562 ms
      </TD>
      <TD>
         3.838 s
      </TD>
   </TR>
</TABLE>

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
         669.5 ns
      </TD>
      <TD CLASS = "thinright">
         4.800 μs
      </TD>
      <TD CLASS = "thinright">
         40.11 μs
      </TD>
      <TD>
         422.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         34.06 ns
      </TD>
      <TD CLASS = "thinright">
         302.1 ns
      </TD>
      <TD CLASS = "thinright">
         3.853 μs
      </TD>
      <TD>
         37.40 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         119.1 ns
      </TD>
      <TD CLASS = "thinright">
         5.462 μs
      </TD>
      <TD CLASS = "thinright">
         193.6 μs
      </TD>
      <TD>
         18.39 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         116.8 ns
      </TD>
      <TD CLASS = "thinright">
         5.120 μs
      </TD>
      <TD CLASS = "thinright">
         191.2 μs
      </TD>
      <TD>
         18.75 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         11.22 ms
      </TD>
      <TD CLASS = "thinright">
         6.592 μs
      </TD>
      <TD CLASS = "thinright">
         90.01 μs
      </TD>
      <TD>
         711.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         899.9 μs
      </TD>
      <TD CLASS = "thinright">
         434.2 ns
      </TD>
      <TD CLASS = "thinright">
         8.455 μs
      </TD>
      <TD>
         80.69 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         525.8 ms
      </TD>
      <TD CLASS = "thinright">
         9.256 μs
      </TD>
      <TD CLASS = "thinright">
         496.6 μs
      </TD>
      <TD>
         17.27 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         546.3 ms
      </TD>
      <TD CLASS = "thinright">
         9.433 μs
      </TD>
      <TD CLASS = "thinright">
         519.5 μs
      </TD>
      <TD>
         18.41 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 62 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 120.32 times faster than Hash-Graph
 * Alga was 18.00 times faster than Hash-Graph
 * Fgl was 1.08 times faster than Hash-Graph

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
         25.54 ns
      </TD>
      <TD CLASS = "thinright">
         76.01 ns
      </TD>
      <TD CLASS = "thinright">
         75.89 ns
      </TD>
      <TD>
         75.84 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         24.76 ns
      </TD>
      <TD CLASS = "thinright">
         12.25 μs
      </TD>
      <TD CLASS = "thinright">
         5.400 ms
      </TD>
      <TD>
         2.571 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.41 ns
      </TD>
      <TD CLASS = "thinright">
         18.64 μs
      </TD>
      <TD CLASS = "thinright">
         7.264 ms
      </TD>
      <TD>
         3.839 s
      </TD>
   </TR>
</TABLE>

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
         25.26 ns
      </TD>
      <TD CLASS = "thinright">
         76.90 ns
      </TD>
      <TD CLASS = "thinright">
         77.24 ns
      </TD>
      <TD>
         75.90 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         28.01 ns
      </TD>
      <TD CLASS = "thinright">
         4.417 μs
      </TD>
      <TD CLASS = "thinright">
         191.2 μs
      </TD>
      <TD>
         18.30 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.24 ns
      </TD>
      <TD CLASS = "thinright">
         4.877 μs
      </TD>
      <TD CLASS = "thinright">
         193.2 μs
      </TD>
      <TD>
         18.77 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         75.51 ns
      </TD>
      <TD CLASS = "thinright">
         75.87 ns
      </TD>
      <TD CLASS = "thinright">
         77.99 ns
      </TD>
      <TD>
         77.87 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         538.8 ms
      </TD>
      <TD CLASS = "thinright">
         7.942 μs
      </TD>
      <TD CLASS = "thinright">
         500.4 μs
      </TD>
      <TD>
         17.17 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         550.9 ms
      </TD>
      <TD CLASS = "thinright">
         9.090 μs
      </TD>
      <TD CLASS = "thinright">
         552.6 μs
      </TD>
      <TD>
         18.59 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 10 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 363490.55 times faster than Hash-Graph
 * Fgl was 1.06 times faster than Hash-Graph

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
         37.52 ns
      </TD>
      <TD CLASS = "thinright">
         1.245 μs
      </TD>
      <TD CLASS = "thinright">
         106.5 μs
      </TD>
      <TD>
         11.55 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         57.44 ns
      </TD>
      <TD CLASS = "thinright">
         16.35 μs
      </TD>
      <TD CLASS = "thinright">
         8.662 ms
      </TD>
      <TD>
         3.349 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         29.24 ns
      </TD>
      <TD CLASS = "thinright">
         18.53 μs
      </TD>
      <TD CLASS = "thinright">
         8.004 ms
      </TD>
      <TD>
         3.435 s
      </TD>
   </TR>
</TABLE>

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
         39.68 ns
      </TD>
      <TD CLASS = "thinright">
         558.5 ns
      </TD>
      <TD CLASS = "thinright">
         7.127 μs
      </TD>
      <TD>
         80.70 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         58.47 ns
      </TD>
      <TD CLASS = "thinright">
         5.013 μs
      </TD>
      <TD CLASS = "thinright">
         195.3 μs
      </TD>
      <TD>
         17.91 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         29.85 ns
      </TD>
      <TD CLASS = "thinright">
         5.301 μs
      </TD>
      <TD CLASS = "thinright">
         193.8 μs
      </TD>
      <TD>
         18.74 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         660.0 μs
      </TD>
      <TD CLASS = "thinright">
         831.5 ns
      </TD>
      <TD CLASS = "thinright">
         11.52 μs
      </TD>
      <TD>
         134.7 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         532.1 ms
      </TD>
      <TD CLASS = "thinright">
         8.778 μs
      </TD>
      <TD CLASS = "thinright">
         494.1 μs
      </TD>
      <TD>
         17.24 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         550.5 ms
      </TD>
      <TD CLASS = "thinright">
         9.215 μs
      </TD>
      <TD CLASS = "thinright">
         523.0 μs
      </TD>
      <TD>
         19.13 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 40 times
 * Hash-Graph was the fastest 2 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 205.07 times faster than Hash-Graph
 * Fgl was 1.12 times faster than Hash-Graph

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
         51.82 ns
      </TD>
      <TD CLASS = "thinright">
         3.460 μs
      </TD>
      <TD CLASS = "thinright">
         366.6 μs
      </TD>
      <TD>
         43.15 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         126.4 ns
      </TD>
      <TD CLASS = "thinright">
         19.07 μs
      </TD>
      <TD CLASS = "thinright">
         9.792 ms
      </TD>
      <TD>
         3.431 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         207.0 ns
      </TD>
      <TD CLASS = "thinright">
         23.09 μs
      </TD>
      <TD CLASS = "thinright">
         7.969 ms
      </TD>
      <TD>
         3.757 s
      </TD>
   </TR>
</TABLE>

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
         51.75 ns
      </TD>
      <TD CLASS = "thinright">
         1.098 μs
      </TD>
      <TD CLASS = "thinright">
         13.44 μs
      </TD>
      <TD>
         142.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         121.1 ns
      </TD>
      <TD CLASS = "thinright">
         6.370 μs
      </TD>
      <TD CLASS = "thinright">
         212.6 μs
      </TD>
      <TD>
         19.02 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         204.2 ns
      </TD>
      <TD CLASS = "thinright">
         7.017 μs
      </TD>
      <TD CLASS = "thinright">
         214.6 μs
      </TD>
      <TD>
         19.33 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         1.983 ms
      </TD>
      <TD CLASS = "thinright">
         1.679 μs
      </TD>
      <TD CLASS = "thinright">
         31.03 μs
      </TD>
      <TD>
         242.7 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         578.6 ms
      </TD>
      <TD CLASS = "thinright">
         10.95 μs
      </TD>
      <TD CLASS = "thinright">
         571.4 μs
      </TD>
      <TD>
         21.05 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         565.4 ms
      </TD>
      <TD CLASS = "thinright">
         12.15 μs
      </TD>
      <TD CLASS = "thinright">
         575.3 μs
      </TD>
      <TD>
         20.18 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 42 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 96.87 times faster than Hash-Graph
 * Fgl was 1.02 times faster than Hash-Graph

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
         41.43 ns
      </TD>
      <TD CLASS = "thinright">
         3.499 μs
      </TD>
      <TD CLASS = "thinright">
         369.6 μs
      </TD>
      <TD>
         39.46 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         74.85 ns
      </TD>
      <TD CLASS = "thinright">
         18.77 μs
      </TD>
      <TD CLASS = "thinright">
         11.26 ms
      </TD>
      <TD>
         3.544 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         124.4 ns
      </TD>
      <TD CLASS = "thinright">
         22.72 μs
      </TD>
      <TD CLASS = "thinright">
         8.439 ms
      </TD>
      <TD>
         3.564 s
      </TD>
   </TR>
</TABLE>

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
         41.57 ns
      </TD>
      <TD CLASS = "thinright">
         1.148 μs
      </TD>
      <TD CLASS = "thinright">
         14.08 μs
      </TD>
      <TD>
         148.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         72.64 ns
      </TD>
      <TD CLASS = "thinright">
         6.365 μs
      </TD>
      <TD CLASS = "thinright">
         217.5 μs
      </TD>
      <TD>
         20.59 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         125.8 ns
      </TD>
      <TD CLASS = "thinright">
         7.145 μs
      </TD>
      <TD CLASS = "thinright">
         216.7 μs
      </TD>
      <TD>
         20.00 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         2.050 ms
      </TD>
      <TD CLASS = "thinright">
         1.605 μs
      </TD>
      <TD CLASS = "thinright">
         30.96 μs
      </TD>
      <TD>
         254.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         616.7 ms
      </TD>
      <TD CLASS = "thinright">
         11.04 μs
      </TD>
      <TD CLASS = "thinright">
         586.7 μs
      </TD>
      <TD>
         22.42 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         562.0 ms
      </TD>
      <TD CLASS = "thinright">
         12.30 μs
      </TD>
      <TD CLASS = "thinright">
         576.0 μs
      </TD>
      <TD>
         20.41 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 24 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 101.15 times faster than Fgl
 * Hash-Graph was 1.04 times faster than Fgl

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
         38.39 ns
      </TD>
      <TD CLASS = "thinright">
         5.877 μs
      </TD>
      <TD CLASS = "thinright">
         1.035 ms
      </TD>
      <TD>
         328.7 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         66.47 ns
      </TD>
      <TD CLASS = "thinright">
         19.56 μs
      </TD>
      <TD CLASS = "thinright">
         11.78 ms
      </TD>
      <TD>
         3.383 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         83.50 ns
      </TD>
      <TD CLASS = "thinright">
         26.45 μs
      </TD>
      <TD CLASS = "thinright">
         8.612 ms
      </TD>
      <TD>
         3.834 s
      </TD>
   </TR>
</TABLE>

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
         39.18 ns
      </TD>
      <TD CLASS = "thinright">
         1.722 μs
      </TD>
      <TD CLASS = "thinright">
         24.50 μs
      </TD>
      <TD>
         318.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         67.09 ns
      </TD>
      <TD CLASS = "thinright">
         6.830 μs
      </TD>
      <TD CLASS = "thinright">
         216.1 μs
      </TD>
      <TD>
         19.67 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         82.79 ns
      </TD>
      <TD CLASS = "thinright">
         8.100 μs
      </TD>
      <TD CLASS = "thinright">
         222.5 μs
      </TD>
      <TD>
         20.96 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         14.78 ms
      </TD>
      <TD CLASS = "thinright">
         2.849 μs
      </TD>
      <TD CLASS = "thinright">
         55.11 μs
      </TD>
      <TD>
         589.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         567.5 ms
      </TD>
      <TD CLASS = "thinright">
         11.49 μs
      </TD>
      <TD CLASS = "thinright">
         584.4 μs
      </TD>
      <TD>
         22.63 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         562.5 ms
      </TD>
      <TD CLASS = "thinright">
         13.49 μs
      </TD>
      <TD CLASS = "thinright">
         579.4 μs
      </TD>
      <TD>
         21.58 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 24 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 23.32 times faster than Hash-Graph
 * Fgl was 1.08 times faster than Hash-Graph

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
         23.20 μs
      </TD>
      <TD CLASS = "thinright">
         9.932 ms
      </TD>
      <TD>
         1.307 s
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         22.51 μs
      </TD>
      <TD CLASS = "thinright">
         10.41 ms
      </TD>
      <TD>
         3.607 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         23.24 μs
      </TD>
      <TD CLASS = "thinright">
         8.386 ms
      </TD>
      <TD>
         3.690 s
      </TD>
   </TR>
</TABLE>

### Mesh
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
         6.911 μs
      </TD>
      <TD CLASS = "thinright">
         94.98 μs
      </TD>
      <TD>
         2.903 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         7.314 μs
      </TD>
      <TD CLASS = "thinright">
         213.9 μs
      </TD>
      <TD>
         18.68 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         6.630 μs
      </TD>
      <TD CLASS = "thinright">
         212.3 μs
      </TD>
      <TD>
         18.76 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         64.16 ms
      </TD>
      <TD CLASS = "thinright">
         10.96 μs
      </TD>
      <TD CLASS = "thinright">
         228.8 μs
      </TD>
      <TD>
         5.759 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         549.5 ms
      </TD>
      <TD CLASS = "thinright">
         11.98 μs
      </TD>
      <TD CLASS = "thinright">
         576.8 μs
      </TD>
      <TD>
         20.32 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         547.1 ms
      </TD>
      <TD CLASS = "thinright">
         11.78 μs
      </TD>
      <TD CLASS = "thinright">
         558.8 μs
      </TD>
      <TD>
         18.58 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 19 times
 * Hash-Graph was the fastest 3 times

 There was 8 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 3.74 times faster than Fgl
 * Hash-Graph was 1.02 times faster than Fgl

## mergeContext

Description: Merge a FGL context in the graph

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
         Fgl
      </TH>
      <TD CLASS = "thinright">
         112.5 ns
      </TD>
      <TD CLASS = "thinright">
         18.36 μs
      </TD>
      <TD CLASS = "thinright">
         11.46 ms
      </TD>
      <TD>
         3.496 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         133.7 ns
      </TD>
      <TD CLASS = "thinright">
         22.38 μs
      </TD>
      <TD CLASS = "thinright">
         8.699 ms
      </TD>
      <TD>
         3.719 s
      </TD>
   </TR>
</TABLE>

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
      <TH CLASS = "thinright">
         100
      </TH>
      <TH>
         1000
      </TH>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         111.8 ns
      </TD>
      <TD CLASS = "thinright">
         6.185 μs
      </TD>
      <TD CLASS = "thinright">
         211.7 μs
      </TD>
      <TD>
         18.49 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         132.2 ns
      </TD>
      <TD CLASS = "thinright">
         6.636 μs
      </TD>
      <TD CLASS = "thinright">
         212.1 μs
      </TD>
      <TD>
         18.67 ms
      </TD>
   </TR>
</TABLE>

### RealLife
<TABLE>
   <TR>
      <TH>
      </TH>
      <TH CLASS = "thinright">
         0
      </TH>
      <TH CLASS = "thinright">
         1
      </TH>
      <TH CLASS = "thinright">
         2
      </TH>
      <TH>
         3
      </TH>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         550.2 ms
      </TD>
      <TD CLASS = "thinright">
         10.88 μs
      </TD>
      <TD CLASS = "thinright">
         574.7 μs
      </TD>
      <TD>
         21.33 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         549.3 ms
      </TD>
      <TD CLASS = "thinright">
         11.78 μs
      </TD>
      <TD CLASS = "thinright">
         575.7 μs
      </TD>
      <TD>
         18.96 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 8 times
 * Hash-Graph was the fastest 6 times

 There was 22 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 1.02 times faster than Fgl


