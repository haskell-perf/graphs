# Benchmarks

Doing:

----
* [addEdge](#addedge)
* [addVertex](#addvertex)
* [dff](#dff)
* [edgeCount](#edgecount)
* [edgeList](#edgelist)
* [equality](#equality)
* [hasEdge](#hasedge)
* [hasVertex](#hasvertex)
* [isEmpty](#isempty)
* [mergeContext](#mergecontext)
* [reachable](#reachable)
* [removeEdge](#removeedge)
* [removeVertex](#removevertex)
* [topSort](#topsort)
* [transpose](#transpose)
* [vertexCount](#vertexcount)
* [vertexList](#vertexlist)
----

Using [("Clique",4),("Mesh",4),("RealLife",4)] as graphs

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
         72.12 ns
      </TD>
      <TD CLASS = "thinright">
         2.758 μs
      </TD>
      <TD CLASS = "thinright">
         304.4 μs
      </TD>
      <TD>
         28.98 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         88.16 ns
      </TD>
      <TD CLASS = "thinright">
         1.297 μs
      </TD>
      <TD CLASS = "thinright">
         121.4 μs
      </TD>
      <TD>
         81.01 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         145.0 ns
      </TD>
      <TD CLASS = "thinright">
         19.18 μs
      </TD>
      <TD CLASS = "thinright">
         9.371 ms
      </TD>
      <TD>
         3.411 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         197.2 ns
      </TD>
      <TD CLASS = "thinright">
         22.71 μs
      </TD>
      <TD CLASS = "thinright">
         7.616 ms
      </TD>
      <TD>
         3.628 s
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
         77.02 ns
      </TD>
      <TD CLASS = "thinright">
         821.0 ns
      </TD>
      <TD CLASS = "thinright">
         10.91 μs
      </TD>
      <TD>
         113.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         89.49 ns
      </TD>
      <TD CLASS = "thinright">
         622.0 ns
      </TD>
      <TD CLASS = "thinright">
         6.193 μs
      </TD>
      <TD>
         73.91 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         148.5 ns
      </TD>
      <TD CLASS = "thinright">
         6.460 μs
      </TD>
      <TD CLASS = "thinright">
         219.8 μs
      </TD>
      <TD>
         19.16 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         197.0 ns
      </TD>
      <TD CLASS = "thinright">
         7.134 μs
      </TD>
      <TD CLASS = "thinright">
         221.1 μs
      </TD>
      <TD>
         19.94 ms
      </TD>
   </TR>
</TABLE>

### RealLife
^[[B^[[B<TABLE>
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
         1.351 μs
      </TD>
      <TD CLASS = "thinright">
         24.70 μs
      </TD>
      <TD CLASS = "thinright">
         194.1 μs
      </TD>
      <TD>
         1.602 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         834.0 ns
      </TD>
      <TD CLASS = "thinright">
         13.31 μs
      </TD>
      <TD CLASS = "thinright">
         113.8 μs
      </TD>
      <TD>
         1.348 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         10.82 μs
      </TD>
      <TD CLASS = "thinright">
         547.1 μs
      </TD>
      <TD CLASS = "thinright">
         20.12 ms
      </TD>
      <TD>
         588.7 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         12.29 μs
      </TD>
      <TD CLASS = "thinright">
         546.2 μs
      </TD>
      <TD CLASS = "thinright">
         18.51 ms
      </TD>
      <TD>
         575.9 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 27 times
 * Alga was the fastest 5 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 151.21 times faster than Hash-Graph
 * Containers was 105.65 times faster than Hash-Graph
 * Fgl was 1.05 times faster than Hash-Graph

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
         54.09 ns
      </TD>
      <TD CLASS = "thinright">
         2.698 μs
      </TD>
      <TD CLASS = "thinright">
         279.8 μs
      </TD>
      <TD>
         26.75 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         107.4 ns
      </TD>
      <TD CLASS = "thinright">
         1.421 μs
      </TD>
      <TD CLASS = "thinright">
         124.0 μs
      </TD>
      <TD>
         88.73 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         87.36 ns
      </TD>
      <TD CLASS = "thinright">
         19.12 μs
      </TD>
      <TD CLASS = "thinright">
         11.10 ms
      </TD>
      <TD>
         3.396 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         116.2 ns
      </TD>
      <TD CLASS = "thinright">
         22.55 μs
      </TD>
      <TD CLASS = "thinright">
         7.917 ms
      </TD>
      <TD>
         3.762 s
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
         54.23 ns
      </TD>
      <TD CLASS = "thinright">
         750.1 ns
      </TD>
      <TD CLASS = "thinright">
         10.08 μs
      </TD>
      <TD>
         109.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         108.0 ns
      </TD>
      <TD CLASS = "thinright">
         739.1 ns
      </TD>
      <TD CLASS = "thinright">
         7.526 μs
      </TD>
      <TD>
         78.35 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         91.16 ns
      </TD>
      <TD CLASS = "thinright">
         6.194 μs
      </TD>
      <TD CLASS = "thinright">
         225.1 μs
      </TD>
      <TD>
         20.05 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         113.3 ns
      </TD>
      <TD CLASS = "thinright">
         6.880 μs
      </TD>
      <TD CLASS = "thinright">
         230.3 μs
      </TD>
      <TD>
         21.06 ms
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
         1.253 μs
      </TD>
      <TD CLASS = "thinright">
         23.21 μs
      </TD>
      <TD CLASS = "thinright">
         182.2 μs
      </TD>
      <TD>
         1.503 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         1.023 μs
      </TD>
      <TD CLASS = "thinright">
         14.46 μs
      </TD>
      <TD CLASS = "thinright">
         121.2 μs
      </TD>
      <TD>
         1.445 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         11.11 μs
      </TD>
      <TD CLASS = "thinright">
         562.2 μs
      </TD>
      <TD CLASS = "thinright">
         21.23 ms
      </TD>
      <TD>
         550.6 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         12.34 μs
      </TD>
      <TD CLASS = "thinright">
         562.7 μs
      </TD>
      <TD CLASS = "thinright">
         19.73 ms
      </TD>
      <TD>
         552.9 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 14 times
 * Alga was the fastest 6 times

 There was 4 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 166.92 times faster than Hash-Graph
 * Containers was 100.20 times faster than Hash-Graph
 * Fgl was 1.09 times faster than Hash-Graph

## dff

Description: Produce a forest, obtained from a DFS (Deep First Search) of each vertex

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
         609.6 ns
      </TD>
      <TD CLASS = "thinright">
         30.67 μs
      </TD>
      <TD CLASS = "thinright">
         6.614 ms
      </TD>
      <TD>
         857.0 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         139.5 ns
      </TD>
      <TD CLASS = "thinright">
         3.219 μs
      </TD>
      <TD CLASS = "thinright">
         343.5 μs
      </TD>
      <TD>
         101.0 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         264.1 ns
      </TD>
      <TD CLASS = "thinright">
         25.25 μs
      </TD>
      <TD CLASS = "thinright">
         11.57 ms
      </TD>
      <TD>
         3.682 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         78.39 ns
      </TD>
      <TD CLASS = "thinright">
         25.85 μs
      </TD>
      <TD CLASS = "thinright">
         9.004 ms
      </TD>
      <TD>
         3.926 s
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
         588.7 ns
      </TD>
      <TD CLASS = "thinright">
         11.32 μs
      </TD>
      <TD CLASS = "thinright">
         165.7 μs
      </TD>
      <TD>
         3.689 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         140.3 ns
      </TD>
      <TD CLASS = "thinright">
         1.719 μs
      </TD>
      <TD CLASS = "thinright">
         18.23 μs
      </TD>
      <TD>
         236.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         259.9 ns
      </TD>
      <TD CLASS = "thinright">
         11.20 μs
      </TD>
      <TD CLASS = "thinright">
         292.3 μs
      </TD>
      <TD>
         19.66 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         73.14 ns
      </TD>
      <TD CLASS = "thinright">
         8.289 μs
      </TD>
      <TD CLASS = "thinright">
         252.1 μs
      </TD>
      <TD>
         20.14 ms
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
         17.29 μs
      </TD>
      <TD CLASS = "thinright">
         400.3 μs
      </TD>
      <TD CLASS = "thinright">
         5.837 ms
      </TD>
      <TD>
         77.33 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         2.568 μs
      </TD>
      <TD CLASS = "thinright">
         32.21 μs
      </TD>
      <TD CLASS = "thinright">
         287.0 μs
      </TD>
      <TD>
         7.036 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         19.11 μs
      </TD>
      <TD CLASS = "thinright">
         690.8 μs
      </TD>
      <TD CLASS = "thinright">
         21.40 ms
      </TD>
      <TD>
         575.6 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         15.11 μs
      </TD>
      <TD CLASS = "thinright">
         586.0 μs
      </TD>
      <TD CLASS = "thinright">
         17.44 ms
      </TD>
      <TD>
         540.3 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 59.45 times faster than Hash-Graph
 * Alga was 5.41 times faster than Hash-Graph
 * Fgl was 1.02 times faster than Hash-Graph

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
         51.68 ns
      </TD>
      <TD CLASS = "thinright">
         17.68 μs
      </TD>
      <TD CLASS = "thinright">
         3.405 ms
      </TD>
      <TD>
         538.9 ms
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
         1.066 μs
      </TD>
      <TD CLASS = "thinright">
         107.9 μs
      </TD>
      <TD>
         70.83 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         50.25 ns
      </TD>
      <TD CLASS = "thinright">
         18.05 μs
      </TD>
      <TD CLASS = "thinright">
         10.05 ms
      </TD>
      <TD>
         3.346 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         55.24 ns
      </TD>
      <TD CLASS = "thinright">
         20.05 μs
      </TD>
      <TD CLASS = "thinright">
         7.648 ms
      </TD>
      <TD>
         3.620 s
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
         51.02 ns
      </TD>
      <TD CLASS = "thinright">
         4.832 μs
      </TD>
      <TD CLASS = "thinright">
         84.94 μs
      </TD>
      <TD>
         1.463 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         40.77 ns
      </TD>
      <TD CLASS = "thinright">
         382.1 ns
      </TD>
      <TD CLASS = "thinright">
         5.082 μs
      </TD>
      <TD>
         53.40 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         44.22 ns
      </TD>
      <TD CLASS = "thinright">
         5.848 μs
      </TD>
      <TD CLASS = "thinright">
         203.3 μs
      </TD>
      <TD>
         18.84 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         52.64 ns
      </TD>
      <TD CLASS = "thinright">
         5.934 μs
      </TD>
      <TD CLASS = "thinright">
         199.1 μs
      </TD>
      <TD>
         19.76 ms
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
         8.552 μs
      </TD>
      <TD CLASS = "thinright">
         253.0 μs
      </TD>
      <TD CLASS = "thinright">
         3.285 ms
      </TD>
      <TD>
         48.20 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         603.0 ns
      </TD>
      <TD CLASS = "thinright">
         10.81 μs
      </TD>
      <TD CLASS = "thinright">
         100.8 μs
      </TD>
      <TD>
         1.174 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         10.32 μs
      </TD>
      <TD CLASS = "thinright">
         503.5 μs
      </TD>
      <TD CLASS = "thinright">
         18.87 ms
      </TD>
      <TD>
         555.9 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         10.47 μs
      </TD>
      <TD CLASS = "thinright">
         496.4 μs
      </TD>
      <TD CLASS = "thinright">
         17.56 ms
      </TD>
      <TD>
         574.3 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 11 times

 There was 1 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 123.40 times faster than Hash-Graph
 * Alga was 7.91 times faster than Hash-Graph
 * Fgl was 1.07 times faster than Hash-Graph

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
         45.02 ns
      </TD>
      <TD CLASS = "thinright">
         17.64 μs
      </TD>
      <TD CLASS = "thinright">
         3.384 ms
      </TD>
      <TD>
         480.9 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         53.74 ns
      </TD>
      <TD CLASS = "thinright">
         1.660 μs
      </TD>
      <TD CLASS = "thinright">
         219.5 μs
      </TD>
      <TD>
         73.73 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         42.50 ns
      </TD>
      <TD CLASS = "thinright">
         18.80 μs
      </TD>
      <TD CLASS = "thinright">
         9.586 ms
      </TD>
      <TD>
         3.290 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         45.45 ns
      </TD>
      <TD CLASS = "thinright">
         20.37 μs
      </TD>
      <TD CLASS = "thinright">
         7.762 ms
      </TD>
      <TD>
         3.738 s
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
         45.04 ns
      </TD>
      <TD CLASS = "thinright">
         4.836 μs
      </TD>
      <TD CLASS = "thinright">
         85.89 μs
      </TD>
      <TD>
         1.412 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         55.53 ns
      </TD>
      <TD CLASS = "thinright">
         643.0 ns
      </TD>
      <TD CLASS = "thinright">
         8.628 μs
      </TD>
      <TD>
         88.20 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         42.61 ns
      </TD>
      <TD CLASS = "thinright">
         6.108 μs
      </TD>
      <TD CLASS = "thinright">
         209.2 μs
      </TD>
      <TD>
         19.97 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         45.29 ns
      </TD>
      <TD CLASS = "thinright">
         5.794 μs
      </TD>
      <TD CLASS = "thinright">
         200.7 μs
      </TD>
      <TD>
         20.17 ms
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
         8.370 μs
      </TD>
      <TD CLASS = "thinright">
         253.3 μs
      </TD>
      <TD CLASS = "thinright">
         3.420 ms
      </TD>
      <TD>
         49.36 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         983.6 ns
      </TD>
      <TD CLASS = "thinright">
         16.28 μs
      </TD>
      <TD CLASS = "thinright">
         148.8 μs
      </TD>
      <TD>
         3.095 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         10.71 μs
      </TD>
      <TD CLASS = "thinright">
         512.2 μs
      </TD>
      <TD CLASS = "thinright">
         19.25 ms
      </TD>
      <TD>
         621.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         10.48 μs
      </TD>
      <TD CLASS = "thinright">
         504.2 μs
      </TD>
      <TD CLASS = "thinright">
         18.31 ms
      </TD>
      <TD>
         563.5 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 101.38 times faster than Hash-Graph
 * Alga was 9.41 times faster than Hash-Graph
 * Fgl was 1.08 times faster than Hash-Graph

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
         151.8 ns
      </TD>
      <TD CLASS = "thinright">
         24.86 μs
      </TD>
      <TD CLASS = "thinright">
         4.723 ms
      </TD>
      <TD>
         694.8 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         41.51 ns
      </TD>
      <TD CLASS = "thinright">
         1.081 μs
      </TD>
      <TD CLASS = "thinright">
         113.7 μs
      </TD>
      <TD>
         80.74 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         181.9 ns
      </TD>
      <TD CLASS = "thinright">
         29.28 μs
      </TD>
      <TD CLASS = "thinright">
         11.70 ms
      </TD>
      <TD>
         3.576 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         79.85 ns
      </TD>
      <TD CLASS = "thinright">
         22.66 μs
      </TD>
      <TD CLASS = "thinright">
         7.920 ms
      </TD>
      <TD>
         3.518 s
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
         153.2 ns
      </TD>
      <TD CLASS = "thinright">
         7.037 μs
      </TD>
      <TD CLASS = "thinright">
         122.6 μs
      </TD>
      <TD>
         2.152 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         40.31 ns
      </TD>
      <TD CLASS = "thinright">
         364.6 ns
      </TD>
      <TD CLASS = "thinright">
         4.846 μs
      </TD>
      <TD>
         46.58 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         181.9 ns
      </TD>
      <TD CLASS = "thinright">
         10.59 μs
      </TD>
      <TD CLASS = "thinright">
         265.8 μs
      </TD>
      <TD>
         19.00 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         79.41 ns
      </TD>
      <TD CLASS = "thinright">
         7.013 μs
      </TD>
      <TD CLASS = "thinright">
         219.6 μs
      </TD>
      <TD>
         18.48 ms
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
         11.50 μs
      </TD>
      <TD CLASS = "thinright">
         358.3 μs
      </TD>
      <TD CLASS = "thinright">
         4.712 ms
      </TD>
      <TD>
         68.09 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         549.0 ns
      </TD>
      <TD CLASS = "thinright">
         10.69 μs
      </TD>
      <TD CLASS = "thinright">
         95.32 μs
      </TD>
      <TD>
         1.155 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         17.08 μs
      </TD>
      <TD CLASS = "thinright">
         616.9 μs
      </TD>
      <TD CLASS = "thinright">
         20.47 ms
      </TD>
      <TD>
         547.5 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         12.09 μs
      </TD>
      <TD CLASS = "thinright">
         546.8 μs
      </TD>
      <TD CLASS = "thinright">
         17.42 ms
      </TD>
      <TD>
         542.7 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 22 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 111.91 times faster than Fgl
 * Alga was 5.96 times faster than Fgl
 * Hash-Graph was 1.03 times faster than Fgl

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
         50.19 ns
      </TD>
      <TD CLASS = "thinright">
         3.352 μs
      </TD>
      <TD CLASS = "thinright">
         498.1 μs
      </TD>
      <TD>
         183.0 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         38.11 ns
      </TD>
      <TD CLASS = "thinright">
         858.1 ns
      </TD>
      <TD CLASS = "thinright">
         90.20 μs
      </TD>
      <TD>
         75.03 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         132.1 ns
      </TD>
      <TD CLASS = "thinright">
         18.19 μs
      </TD>
      <TD CLASS = "thinright">
         8.164 ms
      </TD>
      <TD>
         3.329 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         113.9 ns
      </TD>
      <TD CLASS = "thinright">
         18.74 μs
      </TD>
      <TD CLASS = "thinright">
         7.322 ms
      </TD>
      <TD>
         3.655 s
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
         51.27 ns
      </TD>
      <TD CLASS = "thinright">
         978.7 ns
      </TD>
      <TD CLASS = "thinright">
         11.62 μs
      </TD>
      <TD>
         152.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         38.75 ns
      </TD>
      <TD CLASS = "thinright">
         293.8 ns
      </TD>
      <TD CLASS = "thinright">
         3.655 μs
      </TD>
      <TD>
         40.63 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         125.0 ns
      </TD>
      <TD CLASS = "thinright">
         5.509 μs
      </TD>
      <TD CLASS = "thinright">
         198.3 μs
      </TD>
      <TD>
         19.07 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         118.6 ns
      </TD>
      <TD CLASS = "thinright">
         5.333 μs
      </TD>
      <TD CLASS = "thinright">
         198.2 μs
      </TD>
      <TD>
         19.37 ms
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
         1.473 μs
      </TD>
      <TD CLASS = "thinright">
         25.91 μs
      </TD>
      <TD CLASS = "thinright">
         282.2 μs
      </TD>
      <TD>
         6.916 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         416.8 ns
      </TD>
      <TD CLASS = "thinright">
         7.998 μs
      </TD>
      <TD CLASS = "thinright">
         75.12 μs
      </TD>
      <TD>
         944.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         9.339 μs
      </TD>
      <TD CLASS = "thinright">
         469.4 μs
      </TD>
      <TD CLASS = "thinright">
         16.96 ms
      </TD>
      <TD>
         543.2 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         9.781 μs
      </TD>
      <TD CLASS = "thinright">
         505.0 μs
      </TD>
      <TD CLASS = "thinright">
         18.33 ms
      </TD>
      <TD>
         605.3 ms
      </TD>
   </TR>
</TABLE>


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 124.08 times faster than Hash-Graph
 * Alga was 33.68 times faster than Hash-Graph
 * Fgl was 1.09 times faster than Hash-Graph

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
         36.20 ns
      </TD>
      <TD CLASS = "thinright">
         873.1 ns
      </TD>
      <TD CLASS = "thinright">
         59.47 μs
      </TD>
      <TD>
         6.111 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         33.60 ns
      </TD>
      <TD CLASS = "thinright">
         773.0 ns
      </TD>
      <TD CLASS = "thinright">
         88.78 μs
      </TD>
      <TD>
         70.13 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         56.77 ns
      </TD>
      <TD CLASS = "thinright">
         15.93 μs
      </TD>
      <TD CLASS = "thinright">
         7.607 ms
      </TD>
      <TD>
         3.185 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         37.21 ns
      </TD>
      <TD CLASS = "thinright">
         18.46 μs
      </TD>
      <TD CLASS = "thinright">
         7.435 ms
      </TD>
      <TD>
         3.593 s
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
         35.60 ns
      </TD>
      <TD CLASS = "thinright">
         411.6 ns
      </TD>
      <TD CLASS = "thinright">
         4.024 μs
      </TD>
      <TD>
         45.45 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         32.97 ns
      </TD>
      <TD CLASS = "thinright">
         265.2 ns
      </TD>
      <TD CLASS = "thinright">
         3.583 μs
      </TD>
      <TD>
         43.27 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         56.24 ns
      </TD>
      <TD CLASS = "thinright">
         4.953 μs
      </TD>
      <TD CLASS = "thinright">
         197.9 μs
      </TD>
      <TD>
         18.94 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         40.41 ns
      </TD>
      <TD CLASS = "thinright">
         4.965 μs
      </TD>
      <TD CLASS = "thinright">
         191.7 μs
      </TD>
      <TD>
         20.55 ms
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
         498.7 ns
      </TD>
      <TD CLASS = "thinright">
         5.539 μs
      </TD>
      <TD CLASS = "thinright">
         69.34 μs
      </TD>
      <TD>
         360.9 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         391.7 ns
      </TD>
      <TD CLASS = "thinright">
         8.053 μs
      </TD>
      <TD CLASS = "thinright">
         74.56 μs
      </TD>
      <TD>
         919.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         8.748 μs
      </TD>
      <TD CLASS = "thinright">
         470.6 μs
      </TD>
      <TD CLASS = "thinright">
         16.71 ms
      </TD>
      <TD>
         545.8 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         9.533 μs
      </TD>
      <TD CLASS = "thinright">
         490.6 μs
      </TD>
      <TD CLASS = "thinright">
         18.42 ms
      </TD>
      <TD>
         617.3 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 23 times
 * Containers was the fastest 13 times

 There was 8 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 4251209.16 times faster than Hash-Graph
 * Containers was 129.61 times faster than Hash-Graph
 * Fgl was 1.14 times faster than Hash-Graph

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
         34.26 ns
      </TD>
      <TD CLASS = "thinright">
         55.78 ns
      </TD>
      <TD CLASS = "thinright">
         55.83 ns
      </TD>
      <TD>
         56.21 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         31.80 ns
      </TD>
      <TD CLASS = "thinright">
         784.5 ns
      </TD>
      <TD CLASS = "thinright">
         86.15 μs
      </TD>
      <TD>
         62.05 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         31.41 ns
      </TD>
      <TD CLASS = "thinright">
         12.20 μs
      </TD>
      <TD CLASS = "thinright">
         5.090 ms
      </TD>
      <TD>
         2.598 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         30.53 ns
      </TD>
      <TD CLASS = "thinright">
         19.01 μs
      </TD>
      <TD CLASS = "thinright">
         7.358 ms
      </TD>
      <TD>
         3.660 s
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
         31.98 ns
      </TD>
      <TD CLASS = "thinright">
         57.75 ns
      </TD>
      <TD CLASS = "thinright">
         55.91 ns
      </TD>
      <TD>
         56.82 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         31.21 ns
      </TD>
      <TD CLASS = "thinright">
         270.0 ns
      </TD>
      <TD CLASS = "thinright">
         4.104 μs
      </TD>
      <TD>
         34.66 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         30.86 ns
      </TD>
      <TD CLASS = "thinright">
         4.444 μs
      </TD>
      <TD CLASS = "thinright">
         191.6 μs
      </TD>
      <TD>
         18.52 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         30.57 ns
      </TD>
      <TD CLASS = "thinright">
         4.856 μs
      </TD>
      <TD CLASS = "thinright">
         193.3 μs
      </TD>
      <TD>
         19.90 ms
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
         56.68 ns
      </TD>
      <TD CLASS = "thinright">
         55.96 ns
      </TD>
      <TD CLASS = "thinright">
         56.59 ns
      </TD>
      <TD>
         55.72 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         389.7 ns
      </TD>
      <TD CLASS = "thinright">
         7.943 μs
      </TD>
      <TD CLASS = "thinright">
         76.00 μs
      </TD>
      <TD>
         921.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         7.775 μs
      </TD>
      <TD CLASS = "thinright">
         442.9 μs
      </TD>
      <TD CLASS = "thinright">
         16.03 ms
      </TD>
      <TD>
         591.9 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         9.349 μs
      </TD>
      <TD CLASS = "thinright">
         490.5 μs
      </TD>
      <TD CLASS = "thinright">
         18.49 ms
      </TD>
      <TD>
         547.6 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 10 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 55016533.69 times faster than Hash-Graph
 * Containers was 147.25 times faster than Hash-Graph
 * Fgl was 1.34 times faster than Hash-Graph

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
         127.3 ns
      </TD>
      <TD CLASS = "thinright">
         19.65 μs
      </TD>
      <TD CLASS = "thinright">
         10.84 ms
      </TD>
      <TD>
         3.644 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         134.2 ns
      </TD>
      <TD CLASS = "thinright">
         22.35 μs
      </TD>
      <TD CLASS = "thinright">
         8.275 ms
      </TD>
      <TD>
         3.637 s
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
         122.1 ns
      </TD>
      <TD CLASS = "thinright">
         6.470 μs
      </TD>
      <TD CLASS = "thinright">
         212.2 μs
      </TD>
      <TD>
         18.58 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         135.7 ns
      </TD>
      <TD CLASS = "thinright">
         6.823 μs
      </TD>
      <TD CLASS = "thinright">
         224.1 μs
      </TD>
      <TD>
         18.64 ms
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
         11.14 μs
      </TD>
      <TD CLASS = "thinright">
         524.2 μs
      </TD>
      <TD CLASS = "thinright">
         20.36 ms
      </TD>
      <TD>
         552.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         12.21 μs
      </TD>
      <TD CLASS = "thinright">
         549.4 μs
      </TD>
      <TD CLASS = "thinright">
         20.71 ms
      </TD>
      <TD>
         551.1 ms
      </TD>
   </TR>
</TABLE>

Not implemented for Containers because it is a nonsense.
Not implemented for Alga because it is a nonsense.

SUMMARY:

 * Fgl was the fastest 9 times
 * Hash-Graph was the fastest 4 times

 There was 23 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 1.00 times faster than Fgl

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
         Alga
      </TH>
      <TD CLASS = "thinright">
         802.7 ns
      </TD>
      <TD CLASS = "thinright">
         24.79 μs
      </TD>
      <TD CLASS = "thinright">
         5.076 ms
      </TD>
      <TD>
         725.8 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         141.9 ns
      </TD>
      <TD CLASS = "thinright">
         1.910 μs
      </TD>
      <TD CLASS = "thinright">
         220.0 μs
      </TD>
      <TD>
         77.65 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         335.5 ns
      </TD>
      <TD CLASS = "thinright">
         22.21 μs
      </TD>
      <TD CLASS = "thinright">
         10.38 ms
      </TD>
      <TD>
         3.528 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         76.86 ns
      </TD>
      <TD CLASS = "thinright">
         21.64 μs
      </TD>
      <TD CLASS = "thinright">
         7.713 ms
      </TD>
      <TD>
         3.736 s
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
         795.0 ns
      </TD>
      <TD CLASS = "thinright">
         9.343 μs
      </TD>
      <TD CLASS = "thinright">
         131.2 μs
      </TD>
      <TD>
         2.945 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         140.5 ns
      </TD>
      <TD CLASS = "thinright">
         875.0 ns
      </TD>
      <TD CLASS = "thinright">
         8.857 μs
      </TD>
      <TD>
         106.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         338.8 ns
      </TD>
      <TD CLASS = "thinright">
         9.245 μs
      </TD>
      <TD CLASS = "thinright">
         246.0 μs
      </TD>
      <TD>
         19.03 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         75.63 ns
      </TD>
      <TD CLASS = "thinright">
         6.666 μs
      </TD>
      <TD CLASS = "thinright">
         217.7 μs
      </TD>
      <TD>
         19.42 ms
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
         14.42 μs
      </TD>
      <TD CLASS = "thinright">
         384.6 μs
      </TD>
      <TD CLASS = "thinright">
         4.939 ms
      </TD>
      <TD>
         61.30 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         1.258 μs
      </TD>
      <TD CLASS = "thinright">
         26.84 μs
      </TD>
      <TD CLASS = "thinright">
         165.2 μs
      </TD>
      <TD>
         2.962 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         15.13 μs
      </TD>
      <TD CLASS = "thinright">
         709.4 μs
      </TD>
      <TD CLASS = "thinright">
         19.55 ms
      </TD>
      <TD>
         558.9 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         11.99 μs
      </TD>
      <TD CLASS = "thinright">
         574.3 μs
      </TD>
      <TD CLASS = "thinright">
         17.80 ms
      </TD>
      <TD>
         554.6 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 20 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 96.05 times faster than Hash-Graph
 * Alga was 6.00 times faster than Hash-Graph
 * Fgl was 1.03 times faster than Hash-Graph

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
         18.29 μs
      </TD>
      <TD CLASS = "thinright">
         8.461 ms
      </TD>
      <TD>
         1.277 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         1.457 μs
      </TD>
      <TD CLASS = "thinright">
         116.7 μs
      </TD>
      <TD>
         91.23 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         22.23 μs
      </TD>
      <TD CLASS = "thinright">
         9.930 ms
      </TD>
      <TD>
         3.568 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         22.35 μs
      </TD>
      <TD CLASS = "thinright">
         7.810 ms
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
         6.099 μs
      </TD>
      <TD CLASS = "thinright">
         85.10 μs
      </TD>
      <TD>
         2.551 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         674.7 ns
      </TD>
      <TD CLASS = "thinright">
         6.438 μs
      </TD>
      <TD>
         65.30 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         7.715 μs
      </TD>
      <TD CLASS = "thinright">
         224.4 μs
      </TD>
      <TD>
         19.56 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         7.165 μs
      </TD>
      <TD CLASS = "thinright">
         224.9 μs
      </TD>
      <TD>
         20.05 ms
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
         9.047 μs
      </TD>
      <TD CLASS = "thinright">
         199.9 μs
      </TD>
      <TD CLASS = "thinright">
         5.780 ms
      </TD>
      <TD>
         56.08 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         880.0 ns
      </TD>
      <TD CLASS = "thinright">
         13.73 μs
      </TD>
      <TD CLASS = "thinright">
         115.5 μs
      </TD>
      <TD>
         1.364 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         12.17 μs
      </TD>
      <TD CLASS = "thinright">
         566.8 μs
      </TD>
      <TD CLASS = "thinright">
         20.31 ms
      </TD>
      <TD>
         561.1 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         12.17 μs
      </TD>
      <TD CLASS = "thinright">
         553.2 μs
      </TD>
      <TD CLASS = "thinright">
         18.94 ms
      </TD>
      <TD>
         580.0 ms
      </TD>
   </TR>
</TABLE>


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 98.90 times faster than Hash-Graph
 * Alga was 4.01 times faster than Hash-Graph
 * Fgl was 1.03 times faster than Hash-Graph

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
         41.81 ns
      </TD>
      <TD CLASS = "thinright">
         5.458 μs
      </TD>
      <TD CLASS = "thinright">
         874.1 μs
      </TD>
      <TD>
         261.2 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         45.88 ns
      </TD>
      <TD CLASS = "thinright">
         1.536 μs
      </TD>
      <TD CLASS = "thinright">
         119.4 μs
      </TD>
      <TD>
         75.03 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         95.03 ns
      </TD>
      <TD CLASS = "thinright">
         19.82 μs
      </TD>
      <TD CLASS = "thinright">
         10.55 ms
      </TD>
      <TD>
         3.434 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         89.83 ns
      </TD>
      <TD CLASS = "thinright">
         25.59 μs
      </TD>
      <TD CLASS = "thinright">
         7.803 ms
      </TD>
      <TD>
         3.658 s
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
         42.15 ns
      </TD>
      <TD CLASS = "thinright">
         1.570 μs
      </TD>
      <TD CLASS = "thinright">
         21.49 μs
      </TD>
      <TD>
         267.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         45.51 ns
      </TD>
      <TD CLASS = "thinright">
         857.1 ns
      </TD>
      <TD CLASS = "thinright">
         10.82 μs
      </TD>
      <TD>
         119.7 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         95.39 ns
      </TD>
      <TD CLASS = "thinright">
         6.573 μs
      </TD>
      <TD CLASS = "thinright">
         210.3 μs
      </TD>
      <TD>
         19.54 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         92.46 ns
      </TD>
      <TD CLASS = "thinright">
         8.124 μs
      </TD>
      <TD CLASS = "thinright">
         224.1 μs
      </TD>
      <TD>
         20.75 ms
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
         2.366 μs
      </TD>
      <TD CLASS = "thinright">
         45.80 μs
      </TD>
      <TD CLASS = "thinright">
         491.3 μs
      </TD>
      <TD>
         10.39 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         1.317 μs
      </TD>
      <TD CLASS = "thinright">
         17.24 μs
      </TD>
      <TD CLASS = "thinright">
         134.8 μs
      </TD>
      <TD>
         1.675 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         11.47 μs
      </TD>
      <TD CLASS = "thinright">
         531.2 μs
      </TD>
      <TD CLASS = "thinright">
         19.03 ms
      </TD>
      <TD>
         559.3 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         13.54 μs
      </TD>
      <TD CLASS = "thinright">
         562.8 μs
      </TD>
      <TD CLASS = "thinright">
         18.92 ms
      </TD>
      <TD>
         550.1 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 28 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 102.29 times faster than Hash-Graph
 * Alga was 30.57 times faster than Hash-Graph
 * Fgl was 1.06 times faster than Hash-Graph

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
         Alga
      </TH>
      <TD CLASS = "thinright">
         630.1 ns
      </TD>
      <TD CLASS = "thinright">
         30.49 μs
      </TD>
      <TD CLASS = "thinright">
         6.274 ms
      </TD>
      <TD>
         991.2 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         173.2 ns
      </TD>
      <TD CLASS = "thinright">
         3.374 μs
      </TD>
      <TD CLASS = "thinright">
         349.6 μs
      </TD>
      <TD>
         105.0 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         463.3 ns
      </TD>
      <TD CLASS = "thinright">
         28.19 μs
      </TD>
      <TD CLASS = "thinright">
         10.52 ms
      </TD>
      <TD>
         3.697 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         199.4 ns
      </TD>
      <TD CLASS = "thinright">
         28.38 μs
      </TD>
      <TD CLASS = "thinright">
         7.769 ms
      </TD>
      <TD>
         3.697 s
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
         625.9 ns
      </TD>
      <TD CLASS = "thinright">
         12.33 μs
      </TD>
      <TD CLASS = "thinright">
         185.2 μs
      </TD>
      <TD>
         4.149 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         169.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.882 μs
      </TD>
      <TD CLASS = "thinright">
         19.86 μs
      </TD>
      <TD>
         251.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         460.0 ns
      </TD>
      <TD CLASS = "thinright">
         14.04 μs
      </TD>
      <TD CLASS = "thinright">
         341.9 μs
      </TD>
      <TD>
         24.82 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         206.0 ns
      </TD>
      <TD CLASS = "thinright">
         12.37 μs
      </TD>
      <TD CLASS = "thinright">
         292.8 μs
      </TD>
      <TD>
         21.03 ms
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
         19.00 μs
      </TD>
      <TD CLASS = "thinright">
         400.9 μs
      </TD>
      <TD CLASS = "thinright">
         6.216 ms
      </TD>
      <TD>
         75.18 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         2.815 μs
      </TD>
      <TD CLASS = "thinright">
         32.11 μs
      </TD>
      <TD CLASS = "thinright">
         298.5 μs
      </TD>
      <TD>
         7.486 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         22.97 μs
      </TD>
      <TD CLASS = "thinright">
         793.7 μs
      </TD>
      <TD CLASS = "thinright">
         26.17 ms
      </TD>
      <TD>
         653.8 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         19.88 μs
      </TD>
      <TD CLASS = "thinright">
         531.0 μs
      </TD>
      <TD CLASS = "thinright">
         17.87 ms
      </TD>
      <TD>
         595.1 ms
      </TD>
   </TR>
</TABLE>


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 60.03 times faster than Fgl
 * Alga was 5.32 times faster than Fgl
 * Hash-Graph was 1.06 times faster than Fgl

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
         38.71 ns
      </TD>
      <TD CLASS = "thinright">
         4.515 μs
      </TD>
      <TD CLASS = "thinright">
         497.0 μs
      </TD>
      <TD>
         47.66 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         103.2 ns
      </TD>
      <TD CLASS = "thinright">
         2.952 μs
      </TD>
      <TD CLASS = "thinright">
         466.9 μs
      </TD>
      <TD>
         142.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         134.8 ns
      </TD>
      <TD CLASS = "thinright">
         31.11 μs
      </TD>
      <TD CLASS = "thinright">
         14.24 ms
      </TD>
      <TD>
         3.891 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         69.94 ns
      </TD>
      <TD CLASS = "thinright">
         33.25 μs
      </TD>
      <TD CLASS = "thinright">
         12.24 ms
      </TD>
      <TD>
         4.399 s
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
         38.34 ns
      </TD>
      <TD CLASS = "thinright">
         1.334 μs
      </TD>
      <TD CLASS = "thinright">
         18.05 μs
      </TD>
      <TD>
         196.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         101.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.249 μs
      </TD>
      <TD CLASS = "thinright">
         13.71 μs
      </TD>
      <TD>
         185.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         132.3 ns
      </TD>
      <TD CLASS = "thinright">
         10.21 μs
      </TD>
      <TD CLASS = "thinright">
         264.1 μs
      </TD>
      <TD>
         21.44 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         69.69 ns
      </TD>
      <TD CLASS = "thinright">
         8.615 μs
      </TD>
      <TD CLASS = "thinright">
         251.1 μs
      </TD>
      <TD>
         22.29 ms
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
         2.137 μs
      </TD>
      <TD CLASS = "thinright">
         40.26 μs
      </TD>
      <TD CLASS = "thinright">
         321.6 μs
      </TD>
      <TD>
         2.601 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         1.759 μs
      </TD>
      <TD CLASS = "thinright">
         30.62 μs
      </TD>
      <TD CLASS = "thinright">
         313.4 μs
      </TD>
      <TD>
         6.828 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         17.61 μs
      </TD>
      <TD CLASS = "thinright">
         649.6 μs
      </TD>
      <TD CLASS = "thinright">
         24.12 ms
      </TD>
      <TD>
         714.2 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         15.23 μs
      </TD>
      <TD CLASS = "thinright">
         683.0 μs
      </TD>
      <TD CLASS = "thinright">
         23.38 ms
      </TD>
      <TD>
         582.3 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 4 times
 * Containers was the fastest 4 times

 There was 4 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 125.06 times faster than Hash-Graph
 * Containers was 57.24 times faster than Hash-Graph
 * Fgl was 1.05 times faster than Hash-Graph

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
         44.06 ns
      </TD>
      <TD CLASS = "thinright">
         3.470 μs
      </TD>
      <TD CLASS = "thinright">
         800.4 μs
      </TD>
      <TD>
         204.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         32.68 ns
      </TD>
      <TD CLASS = "thinright">
         996.0 ns
      </TD>
      <TD CLASS = "thinright">
         85.82 μs
      </TD>
      <TD>
         84.43 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         33.80 ns
      </TD>
      <TD CLASS = "thinright">
         12.86 μs
      </TD>
      <TD CLASS = "thinright">
         4.997 ms
      </TD>
      <TD>
         2.707 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         35.17 ns
      </TD>
      <TD CLASS = "thinright">
         18.57 μs
      </TD>
      <TD CLASS = "thinright">
         7.493 ms
      </TD>
      <TD>
         3.679 s
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
         44.14 ns
      </TD>
      <TD CLASS = "thinright">
         979.0 ns
      </TD>
      <TD CLASS = "thinright">
         14.77 μs
      </TD>
      <TD>
         248.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         32.98 ns
      </TD>
      <TD CLASS = "thinright">
         268.2 ns
      </TD>
      <TD CLASS = "thinright">
         3.834 μs
      </TD>
      <TD>
         33.93 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         33.80 ns
      </TD>
      <TD CLASS = "thinright">
         4.561 μs
      </TD>
      <TD CLASS = "thinright">
         191.8 μs
      </TD>
      <TD>
         18.48 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         35.03 ns
      </TD>
      <TD CLASS = "thinright">
         5.149 μs
      </TD>
      <TD CLASS = "thinright">
         192.5 μs
      </TD>
      <TD>
         18.53 ms
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
         1.582 μs
      </TD>
      <TD CLASS = "thinright">
         41.72 μs
      </TD>
      <TD CLASS = "thinright">
         701.1 μs
      </TD>
      <TD>
         11.45 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         391.6 ns
      </TD>
      <TD CLASS = "thinright">
         7.954 μs
      </TD>
      <TD CLASS = "thinright">
         74.28 μs
      </TD>
      <TD>
         929.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         8.252 μs
      </TD>
      <TD CLASS = "thinright">
         443.8 μs
      </TD>
      <TD CLASS = "thinright">
         16.06 ms
      </TD>
      <TD>
         521.7 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         9.660 μs
      </TD>
      <TD CLASS = "thinright">
         494.5 μs
      </TD>
      <TD CLASS = "thinright">
         17.94 ms
      </TD>
      <TD>
         566.9 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 113.55 times faster than Hash-Graph
 * Alga was 23.88 times faster than Hash-Graph
 * Fgl was 1.32 times faster than Hash-Graph

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
         52.50 ns
      </TD>
      <TD CLASS = "thinright">
         3.676 μs
      </TD>
      <TD CLASS = "thinright">
         743.7 μs
      </TD>
      <TD>
         225.0 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         44.04 ns
      </TD>
      <TD CLASS = "thinright">
         907.4 ns
      </TD>
      <TD CLASS = "thinright">
         136.8 μs
      </TD>
      <TD>
         75.21 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         60.56 ns
      </TD>
      <TD CLASS = "thinright">
         15.03 μs
      </TD>
      <TD CLASS = "thinright">
         6.064 ms
      </TD>
      <TD>
         2.841 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         35.31 ns
      </TD>
      <TD CLASS = "thinright">
         18.65 μs
      </TD>
      <TD CLASS = "thinright">
         8.874 ms
      </TD>
      <TD>
         3.599 s
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
         51.06 ns
      </TD>
      <TD CLASS = "thinright">
         1.093 μs
      </TD>
      <TD CLASS = "thinright">
         15.61 μs
      </TD>
      <TD>
         255.7 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         43.38 ns
      </TD>
      <TD CLASS = "thinright">
         395.4 ns
      </TD>
      <TD CLASS = "thinright">
         4.234 μs
      </TD>
      <TD>
         44.22 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         60.50 ns
      </TD>
      <TD CLASS = "thinright">
         5.339 μs
      </TD>
      <TD CLASS = "thinright">
         201.9 μs
      </TD>
      <TD>
         18.12 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         34.43 ns
      </TD>
      <TD CLASS = "thinright">
         5.275 μs
      </TD>
      <TD CLASS = "thinright">
         191.5 μs
      </TD>
      <TD>
         17.87 ms
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
         1.792 μs
      </TD>
      <TD CLASS = "thinright">
         42.48 μs
      </TD>
      <TD CLASS = "thinright">
         672.2 μs
      </TD>
      <TD>
         11.51 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         553.8 ns
      </TD>
      <TD CLASS = "thinright">
         8.867 μs
      </TD>
      <TD CLASS = "thinright">
         77.89 μs
      </TD>
      <TD>
         931.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         9.500 μs
      </TD>
      <TD CLASS = "thinright">
         473.0 μs
      </TD>
      <TD CLASS = "thinright">
         16.19 ms
      </TD>
      <TD>
         529.2 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         9.491 μs
      </TD>
      <TD CLASS = "thinright">
         482.9 μs
      </TD>
      <TD CLASS = "thinright">
         17.04 ms
      </TD>
      <TD>
         540.7 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 119.57 times faster than Hash-Graph
 * Alga was 23.44 times faster than Hash-Graph
 * Fgl was 1.22 times faster than Hash-Graph

