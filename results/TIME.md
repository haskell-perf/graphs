The content of this file was obtained with:

```
$ time run -g '("Clique",4)' -g '("Mesh",4)' -g '("RealLife",4)' -d Html
```

# Benchmarks

Doing:

----
* [addEdge](#addedge)
* [addVertex](#addvertex)
* [creation](#creation)
* [dff](#dff)
* [edgeList](#edgelist)
* [edgeCount](#edgecount)
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
         53.40 ns
      </TD>
      <TD CLASS = "thinright">
         1.272 μs
      </TD>
      <TD CLASS = "thinright">
         128.7 μs
      </TD>
      <TD>
         15.40 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         76.21 ns
      </TD>
      <TD CLASS = "thinright">
         535.5 ns
      </TD>
      <TD CLASS = "thinright">
         38.57 μs
      </TD>
      <TD>
         23.57 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         120.6 ns
      </TD>
      <TD CLASS = "thinright">
         2.095 μs
      </TD>
      <TD CLASS = "thinright">
         190.9 μs
      </TD>
      <TD>
         28.44 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         170.4 ns
      </TD>
      <TD CLASS = "thinright">
         4.088 μs
      </TD>
      <TD CLASS = "thinright">
         385.9 μs
      </TD>
      <TD>
         43.19 ms
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
         56.80 ns
      </TD>
      <TD CLASS = "thinright">
         451.5 ns
      </TD>
      <TD CLASS = "thinright">
         4.493 μs
      </TD>
      <TD>
         52.12 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         72.67 ns
      </TD>
      <TD CLASS = "thinright">
         348.7 ns
      </TD>
      <TD CLASS = "thinright">
         2.795 μs
      </TD>
      <TD>
         27.69 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         118.6 ns
      </TD>
      <TD CLASS = "thinright">
         981.5 ns
      </TD>
      <TD CLASS = "thinright">
         9.440 μs
      </TD>
      <TD>
         94.10 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         172.5 ns
      </TD>
      <TD CLASS = "thinright">
         1.789 μs
      </TD>
      <TD CLASS = "thinright">
         19.94 μs
      </TD>
      <TD>
         234.0 μs
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
         680.4 ns
      </TD>
      <TD CLASS = "thinright">
         11.36 μs
      </TD>
      <TD CLASS = "thinright">
         88.76 μs
      </TD>
      <TD>
         704.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         452.1 ns
      </TD>
      <TD CLASS = "thinright">
         5.053 μs
      </TD>
      <TD CLASS = "thinright">
         30.46 μs
      </TD>
      <TD>
         218.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.397 μs
      </TD>
      <TD CLASS = "thinright">
         19.49 μs
      </TD>
      <TD CLASS = "thinright">
         143.8 μs
      </TD>
      <TD>
         1.302 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.598 μs
      </TD>
      <TD CLASS = "thinright">
         37.63 μs
      </TD>
      <TD CLASS = "thinright">
         291.1 μs
      </TD>
      <TD>
         2.461 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 27 times
 * Alga was the fastest 5 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 3.99 times faster than Hash-Graph
 * Alga was 3.21 times faster than Hash-Graph
 * Fgl was 1.71 times faster than Hash-Graph

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
         34.43 ns
      </TD>
      <TD CLASS = "thinright">
         1.063 μs
      </TD>
      <TD CLASS = "thinright">
         116.0 μs
      </TD>
      <TD>
         13.07 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         94.72 ns
      </TD>
      <TD CLASS = "thinright">
         638.3 ns
      </TD>
      <TD CLASS = "thinright">
         39.69 μs
      </TD>
      <TD>
         23.10 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         76.40 ns
      </TD>
      <TD CLASS = "thinright">
         1.927 μs
      </TD>
      <TD CLASS = "thinright">
         194.9 μs
      </TD>
      <TD>
         31.37 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         105.5 ns
      </TD>
      <TD CLASS = "thinright">
         3.527 μs
      </TD>
      <TD CLASS = "thinright">
         320.9 μs
      </TD>
      <TD>
         36.07 ms
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
         34.38 ns
      </TD>
      <TD CLASS = "thinright">
         332.8 ns
      </TD>
      <TD CLASS = "thinright">
         4.161 μs
      </TD>
      <TD>
         44.81 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         93.73 ns
      </TD>
      <TD CLASS = "thinright">
         456.6 ns
      </TD>
      <TD CLASS = "thinright">
         3.678 μs
      </TD>
      <TD>
         37.01 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         74.05 ns
      </TD>
      <TD CLASS = "thinright">
         804.8 ns
      </TD>
      <TD CLASS = "thinright">
         9.292 μs
      </TD>
      <TD>
         95.34 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         102.9 ns
      </TD>
      <TD CLASS = "thinright">
         1.488 μs
      </TD>
      <TD CLASS = "thinright">
         17.93 μs
      </TD>
      <TD>
         207.4 μs
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
         510.4 ns
      </TD>
      <TD CLASS = "thinright">
         9.150 μs
      </TD>
      <TD CLASS = "thinright">
         72.56 μs
      </TD>
      <TD>
         612.6 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         606.0 ns
      </TD>
      <TD CLASS = "thinright">
         5.986 μs
      </TD>
      <TD CLASS = "thinright">
         33.81 μs
      </TD>
      <TD>
         240.6 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.248 μs
      </TD>
      <TD CLASS = "thinright">
         19.05 μs
      </TD>
      <TD CLASS = "thinright">
         140.8 μs
      </TD>
      <TD>
         1.312 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.188 μs
      </TD>
      <TD CLASS = "thinright">
         32.94 μs
      </TD>
      <TD CLASS = "thinright">
         255.6 μs
      </TD>
      <TD>
         2.169 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 14 times
 * Alga was the fastest 10 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 3.23 times faster than Hash-Graph
 * Alga was 3.21 times faster than Hash-Graph
 * Fgl was 1.41 times faster than Hash-Graph

## creation

Description: Create a graph from a list of edges

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
         13.88 ns
      </TD>
      <TD CLASS = "thinright">
         2.323 μs
      </TD>
      <TD CLASS = "thinright">
         251.4 μs
      </TD>
      <TD>
         24.97 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         50.50 ns
      </TD>
      <TD CLASS = "thinright">
         1.243 μs
      </TD>
      <TD CLASS = "thinright">
         113.8 μs
      </TD>
      <TD>
         77.45 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         39.34 ns
      </TD>
      <TD CLASS = "thinright">
         18.45 μs
      </TD>
      <TD CLASS = "thinright">
         9.162 ms
      </TD>
      <TD>
         3.363 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         28.50 ns
      </TD>
      <TD CLASS = "thinright">
         21.99 μs
      </TD>
      <TD CLASS = "thinright">
         7.145 ms
      </TD>
      <TD>
         3.608 s
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
         13.70 ns
      </TD>
      <TD CLASS = "thinright">
         698.7 ns
      </TD>
      <TD CLASS = "thinright">
         9.213 μs
      </TD>
      <TD>
         101.9 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         49.45 ns
      </TD>
      <TD CLASS = "thinright">
         527.6 ns
      </TD>
      <TD CLASS = "thinright">
         5.944 μs
      </TD>
      <TD>
         76.86 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         39.17 ns
      </TD>
      <TD CLASS = "thinright">
         6.064 μs
      </TD>
      <TD CLASS = "thinright">
         206.3 μs
      </TD>
      <TD>
         19.33 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         28.44 ns
      </TD>
      <TD CLASS = "thinright">
         6.237 μs
      </TD>
      <TD CLASS = "thinright">
         209.3 μs
      </TD>
      <TD>
         18.92 ms
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
         1.091 μs
      </TD>
      <TD CLASS = "thinright">
         20.65 μs
      </TD>
      <TD CLASS = "thinright">
         168.3 μs
      </TD>
      <TD>
         1.362 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         764.5 ns
      </TD>
      <TD CLASS = "thinright">
         13.43 μs
      </TD>
      <TD CLASS = "thinright">
         113.6 μs
      </TD>
      <TD>
         1.350 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         10.51 μs
      </TD>
      <TD CLASS = "thinright">
         520.4 μs
      </TD>
      <TD CLASS = "thinright">
         18.61 ms
      </TD>
      <TD>
         594.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         11.20 μs
      </TD>
      <TD CLASS = "thinright">
         526.2 μs
      </TD>
      <TD CLASS = "thinright">
         17.41 ms
      </TD>
      <TD>
         573.6 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 8 times
 * Alga was the fastest 3 times

 There was 1 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 173.83 times faster than Hash-Graph
 * Containers was 107.58 times faster than Hash-Graph
 * Fgl was 1.05 times faster than Hash-Graph

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
         548.1 ns
      </TD>
      <TD CLASS = "thinright">
         26.29 μs
      </TD>
      <TD CLASS = "thinright">
         7.183 ms
      </TD>
      <TD>
         598.9 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         114.1 ns
      </TD>
      <TD CLASS = "thinright">
         2.453 μs
      </TD>
      <TD CLASS = "thinright">
         169.7 μs
      </TD>
      <TD>
         29.45 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         210.6 ns
      </TD>
      <TD CLASS = "thinright">
         6.052 μs
      </TD>
      <TD CLASS = "thinright">
         572.3 μs
      </TD>
      <TD>
         121.0 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         64.63 ns
      </TD>
      <TD CLASS = "thinright">
         5.784 μs
      </TD>
      <TD CLASS = "thinright">
         569.3 μs
      </TD>
      <TD>
         96.54 ms
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
         543.4 ns
      </TD>
      <TD CLASS = "thinright">
         10.23 μs
      </TD>
      <TD CLASS = "thinright">
         154.1 μs
      </TD>
      <TD>
         2.966 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         113.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.451 μs
      </TD>
      <TD CLASS = "thinright">
         14.56 μs
      </TD>
      <TD>
         161.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         208.3 ns
      </TD>
      <TD CLASS = "thinright">
         4.572 μs
      </TD>
      <TD CLASS = "thinright">
         70.32 μs
      </TD>
      <TD>
         930.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         64.58 ns
      </TD>
      <TD CLASS = "thinright">
         2.644 μs
      </TD>
      <TD CLASS = "thinright">
         50.58 μs
      </TD>
      <TD>
         1.469 ms
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
         16.17 μs
      </TD>
      <TD CLASS = "thinright">
         363.3 μs
      </TD>
      <TD CLASS = "thinright">
         4.839 ms
      </TD>
      <TD>
         54.68 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         2.209 μs
      </TD>
      <TD CLASS = "thinright">
         23.84 μs
      </TD>
      <TD CLASS = "thinright">
         165.5 μs
      </TD>
      <TD>
         3.532 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         7.365 μs
      </TD>
      <TD CLASS = "thinright">
         142.2 μs
      </TD>
      <TD CLASS = "thinright">
         1.628 ms
      </TD>
      <TD>
         17.13 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         4.648 μs
      </TD>
      <TD CLASS = "thinright">
         68.40 μs
      </TD>
      <TD CLASS = "thinright">
         180.3 ns
      </TD>
      <TD>
         35.86 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 8 times
 * Hash-Graph was the fastest 4 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 587.63 times faster than Alga
 * Containers was 17.82 times faster than Alga
 * Fgl was 3.70 times faster than Alga

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
         39.79 ns
      </TD>
      <TD CLASS = "thinright">
         15.77 μs
      </TD>
      <TD CLASS = "thinright">
         2.403 ms
      </TD>
      <TD>
         514.6 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         29.08 ns
      </TD>
      <TD CLASS = "thinright">
         287.4 ns
      </TD>
      <TD CLASS = "thinright">
         27.20 μs
      </TD>
      <TD>
         25.07 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         35.80 ns
      </TD>
      <TD CLASS = "thinright">
         1.862 μs
      </TD>
      <TD CLASS = "thinright">
         199.7 μs
      </TD>
      <TD>
         134.1 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         39.28 ns
      </TD>
      <TD CLASS = "thinright">
         993.7 ns
      </TD>
      <TD CLASS = "thinright">
         58.66 μs
      </TD>
      <TD>
         7.000 ms
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
         39.26 ns
      </TD>
      <TD CLASS = "thinright">
         4.476 μs
      </TD>
      <TD CLASS = "thinright">
         78.24 μs
      </TD>
      <TD>
         1.138 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         29.42 ns
      </TD>
      <TD CLASS = "thinright">
         128.8 ns
      </TD>
      <TD CLASS = "thinright">
         1.190 μs
      </TD>
      <TD>
         12.25 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         34.34 ns
      </TD>
      <TD CLASS = "thinright">
         678.3 ns
      </TD>
      <TD CLASS = "thinright">
         8.998 μs
      </TD>
      <TD>
         97.19 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         39.35 ns
      </TD>
      <TD CLASS = "thinright">
         727.2 ns
      </TD>
      <TD CLASS = "thinright">
         8.851 μs
      </TD>
      <TD>
         112.8 μs
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
         7.845 μs
      </TD>
      <TD CLASS = "thinright">
         237.5 μs
      </TD>
      <TD CLASS = "thinright">
         2.916 ms
      </TD>
      <TD>
         43.71 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         226.7 ns
      </TD>
      <TD CLASS = "thinright">
         2.355 μs
      </TD>
      <TD CLASS = "thinright">
         18.51 μs
      </TD>
      <TD>
         137.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.018 μs
      </TD>
      <TD CLASS = "thinright">
         18.49 μs
      </TD>
      <TD CLASS = "thinright">
         141.6 μs
      </TD>
      <TD>
         3.658 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1.175 μs
      </TD>
      <TD CLASS = "thinright">
         11.72 μs
      </TD>
      <TD CLASS = "thinright">
         78.28 μs
      </TD>
      <TD>
         537.1 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 11 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 63.01 times faster than Alga
 * Containers was 47.91 times faster than Alga
 * Fgl was 5.60 times faster than Alga


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
         34.92 ns
      </TD>
      <TD CLASS = "thinright">
         15.55 μs
      </TD>
      <TD CLASS = "thinright">
         2.355 ms
      </TD>
      <TD>
         417.3 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         36.99 ns
      </TD>
      <TD CLASS = "thinright">
         855.6 ns
      </TD>
      <TD CLASS = "thinright">
         77.65 μs
      </TD>
      <TD>
         24.32 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         33.57 ns
      </TD>
      <TD CLASS = "thinright">
         2.842 μs
      </TD>
      <TD CLASS = "thinright">
         340.0 μs
      </TD>
      <TD>
         124.7 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         35.24 ns
      </TD>
      <TD CLASS = "thinright">
         1.817 μs
      </TD>
      <TD CLASS = "thinright">
         307.3 μs
      </TD>
      <TD>
         221.1 ms
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
         34.91 ns
      </TD>
      <TD CLASS = "thinright">
         4.427 μs
      </TD>
      <TD CLASS = "thinright">
         77.78 μs
      </TD>
      <TD>
         1.200 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         37.44 ns
      </TD>
      <TD CLASS = "thinright">
         390.0 ns
      </TD>
      <TD CLASS = "thinright">
         4.218 μs
      </TD>
      <TD>
         44.52 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         32.72 ns
      </TD>
      <TD CLASS = "thinright">
         1.045 μs
      </TD>
      <TD CLASS = "thinright">
         12.58 μs
      </TD>
      <TD>
         137.9 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         35.30 ns
      </TD>
      <TD CLASS = "thinright">
         686.0 ns
      </TD>
      <TD CLASS = "thinright">
         9.494 μs
      </TD>
      <TD>
         148.3 μs
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
         7.703 μs
      </TD>
      <TD CLASS = "thinright">
         227.5 μs
      </TD>
      <TD CLASS = "thinright">
         2.817 ms
      </TD>
      <TD>
         42.66 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         568.2 ns
      </TD>
      <TD CLASS = "thinright">
         7.750 μs
      </TD>
      <TD CLASS = "thinright">
         54.62 μs
      </TD>
      <TD>
         452.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.644 μs
      </TD>
      <TD CLASS = "thinright">
         26.52 μs
      </TD>
      <TD CLASS = "thinright">
         203.5 μs
      </TD>
      <TD>
         4.836 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1.016 μs
      </TD>
      <TD CLASS = "thinright">
         19.87 μs
      </TD>
      <TD CLASS = "thinright">
         205.8 μs
      </TD>
      <TD>
         8.480 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 28.31 times faster than Alga
 * Fgl was 5.77 times faster than Alga
 * Hash-Graph was 4.37 times faster than Alga

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
         143.3 ns
      </TD>
      <TD CLASS = "thinright">
         23.60 μs
      </TD>
      <TD CLASS = "thinright">
         3.418 ms
      </TD>
      <TD>
         480.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         28.81 ns
      </TD>
      <TD CLASS = "thinright">
         265.7 ns
      </TD>
      <TD CLASS = "thinright">
         24.33 μs
      </TD>
      <TD>
         10.12 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         163.6 ns
      </TD>
      <TD CLASS = "thinright">
         14.02 μs
      </TD>
      <TD CLASS = "thinright">
         2.586 ms
      </TD>
      <TD>
         516.1 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         65.44 ns
      </TD>
      <TD CLASS = "thinright">
         4.298 μs
      </TD>
      <TD CLASS = "thinright">
         445.4 μs
      </TD>
      <TD>
         57.55 ms
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
         142.4 ns
      </TD>
      <TD CLASS = "thinright">
         6.585 μs
      </TD>
      <TD CLASS = "thinright">
         109.6 μs
      </TD>
      <TD>
         1.566 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         28.23 ns
      </TD>
      <TD CLASS = "thinright">
         120.5 ns
      </TD>
      <TD CLASS = "thinright">
         1.120 μs
      </TD>
      <TD>
         11.86 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         159.4 ns
      </TD>
      <TD CLASS = "thinright">
         4.927 μs
      </TD>
      <TD CLASS = "thinright">
         61.38 μs
      </TD>
      <TD>
         876.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         63.93 ns
      </TD>
      <TD CLASS = "thinright">
         1.802 μs
      </TD>
      <TD CLASS = "thinright">
         21.34 μs
      </TD>
      <TD>
         313.2 μs
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
         11.00 μs
      </TD>
      <TD CLASS = "thinright">
         331.1 μs
      </TD>
      <TD CLASS = "thinright">
         3.898 ms
      </TD>
      <TD>
         53.98 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         176.8 ns
      </TD>
      <TD CLASS = "thinright">
         2.547 μs
      </TD>
      <TD CLASS = "thinright">
         18.16 μs
      </TD>
      <TD>
         161.6 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         7.555 μs
      </TD>
      <TD CLASS = "thinright">
         130.4 μs
      </TD>
      <TD CLASS = "thinright">
         1.911 ms
      </TD>
      <TD>
         25.34 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.687 μs
      </TD>
      <TD CLASS = "thinright">
         42.88 μs
      </TD>
      <TD CLASS = "thinright">
         342.5 μs
      </TD>
      <TD>
         3.117 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 22 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 145.15 times faster than Fgl
 * Hash-Graph was 19.17 times faster than Fgl
 * Alga was 1.14 times faster than Fgl

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
         40.06 ns
      </TD>
      <TD CLASS = "thinright">
         1.590 μs
      </TD>
      <TD CLASS = "thinright">
         177.7 μs
      </TD>
      <TD>
         53.41 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         28.61 ns
      </TD>
      <TD CLASS = "thinright">
         108.9 ns
      </TD>
      <TD CLASS = "thinright">
         1.015 μs
      </TD>
      <TD>
         10.31 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         118.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.354 μs
      </TD>
      <TD CLASS = "thinright">
         18.42 μs
      </TD>
      <TD>
         457.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         102.5 ns
      </TD>
      <TD CLASS = "thinright">
         145.3 ns
      </TD>
      <TD CLASS = "thinright">
         159.6 ns
      </TD>
      <TD>
         171.0 ns
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
         39.76 ns
      </TD>
      <TD CLASS = "thinright">
         497.1 ns
      </TD>
      <TD CLASS = "thinright">
         4.412 μs
      </TD>
      <TD>
         62.57 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         28.51 ns
      </TD>
      <TD CLASS = "thinright">
         48.56 ns
      </TD>
      <TD CLASS = "thinright">
         50.76 ns
      </TD>
      <TD>
         48.53 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         118.2 ns
      </TD>
      <TD CLASS = "thinright">
         466.5 ns
      </TD>
      <TD CLASS = "thinright">
         589.4 ns
      </TD>
      <TD>
         743.1 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         101.2 ns
      </TD>
      <TD CLASS = "thinright">
         142.6 ns
      </TD>
      <TD CLASS = "thinright">
         149.0 ns
      </TD>
      <TD>
         160.9 ns
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
         699.8 ns
      </TD>
      <TD CLASS = "thinright">
         10.18 μs
      </TD>
      <TD CLASS = "thinright">
         111.7 μs
      </TD>
      <TD>
         1.133 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         52.76 ns
      </TD>
      <TD CLASS = "thinright">
         71.55 ns
      </TD>
      <TD CLASS = "thinright">
         179.3 ns
      </TD>
      <TD>
         115.8 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         515.3 ns
      </TD>
      <TD CLASS = "thinright">
         1.490 μs
      </TD>
      <TD CLASS = "thinright">
         8.264 μs
      </TD>
      <TD>
         8.327 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         138.6 ns
      </TD>
      <TD CLASS = "thinright">
         151.8 ns
      </TD>
      <TD CLASS = "thinright">
         159.5 ns
      </TD>
      <TD>
         163.0 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 44 times
 * Hash-Graph was the fastest 16 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 60515.77 times faster than Alga
 * Containers was 2735.10 times faster than Alga
 * Fgl was 103.59 times faster than Alga

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
         23.60 ns
      </TD>
      <TD CLASS = "thinright">
         434.6 ns
      </TD>
      <TD CLASS = "thinright">
         40.71 μs
      </TD>
      <TD>
         3.220 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         21.96 ns
      </TD>
      <TD CLASS = "thinright">
         22.53 ns
      </TD>
      <TD CLASS = "thinright">
         21.98 ns
      </TD>
      <TD>
         21.84 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         45.35 ns
      </TD>
      <TD CLASS = "thinright">
         675.3 ns
      </TD>
      <TD CLASS = "thinright">
         11.58 μs
      </TD>
      <TD>
         328.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.95 ns
      </TD>
      <TD CLASS = "thinright">
         31.90 ns
      </TD>
      <TD CLASS = "thinright">
         36.73 ns
      </TD>
      <TD>
         42.01 ns
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
         23.37 ns
      </TD>
      <TD CLASS = "thinright">
         245.6 ns
      </TD>
      <TD CLASS = "thinright">
         2.418 μs
      </TD>
      <TD>
         28.08 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         21.94 ns
      </TD>
      <TD CLASS = "thinright">
         21.89 ns
      </TD>
      <TD CLASS = "thinright">
         22.08 ns
      </TD>
      <TD>
         22.08 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         43.62 ns
      </TD>
      <TD CLASS = "thinright">
         227.8 ns
      </TD>
      <TD CLASS = "thinright">
         381.5 ns
      </TD>
      <TD>
         612.1 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.63 ns
      </TD>
      <TD CLASS = "thinright">
         35.64 ns
      </TD>
      <TD CLASS = "thinright">
         37.29 ns
      </TD>
      <TD>
         42.43 ns
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
         274.2 ns
      </TD>
      <TD CLASS = "thinright">
         2.671 μs
      </TD>
      <TD CLASS = "thinright">
         39.84 μs
      </TD>
      <TD>
         232.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         22.76 ns
      </TD>
      <TD CLASS = "thinright">
         22.14 ns
      </TD>
      <TD CLASS = "thinright">
         22.46 ns
      </TD>
      <TD>
         22.03 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         269.2 ns
      </TD>
      <TD CLASS = "thinright">
         1.031 μs
      </TD>
      <TD CLASS = "thinright">
         2.569 μs
      </TD>
      <TD>
         3.454 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         32.64 ns
      </TD>
      <TD CLASS = "thinright">
         38.79 ns
      </TD>
      <TD CLASS = "thinright">
         35.49 ns
      </TD>
      <TD>
         41.28 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 38 times

 There was 6 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 34.99 times faster than Alga
 * Hash-Graph was 18.68 times faster than Alga
 * Fgl was 4.68 times faster than Alga

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
         19.58 ns
      </TD>
      <TD CLASS = "thinright">
         30.63 ns
      </TD>
      <TD CLASS = "thinright">
         30.58 ns
      </TD>
      <TD>
         31.47 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         19.22 ns
      </TD>
      <TD CLASS = "thinright">
         18.39 ns
      </TD>
      <TD CLASS = "thinright">
         18.47 ns
      </TD>
      <TD>
         18.35 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         18.68 ns
      </TD>
      <TD CLASS = "thinright">
         17.97 ns
      </TD>
      <TD CLASS = "thinright">
         18.17 ns
      </TD>
      <TD>
         18.76 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         18.05 ns
      </TD>
      <TD CLASS = "thinright">
         17.91 ns
      </TD>
      <TD CLASS = "thinright">
         17.88 ns
      </TD>
      <TD>
         17.96 ns
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
         19.15 ns
      </TD>
      <TD CLASS = "thinright">
         30.79 ns
      </TD>
      <TD CLASS = "thinright">
         30.72 ns
      </TD>
      <TD>
         30.72 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         18.67 ns
      </TD>
      <TD CLASS = "thinright">
         18.40 ns
      </TD>
      <TD CLASS = "thinright">
         18.54 ns
      </TD>
      <TD>
         18.68 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         17.95 ns
      </TD>
      <TD CLASS = "thinright">
         18.07 ns
      </TD>
      <TD CLASS = "thinright">
         18.07 ns
      </TD>
      <TD>
         18.20 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         18.09 ns
      </TD>
      <TD CLASS = "thinright">
         17.98 ns
      </TD>
      <TD CLASS = "thinright">
         18.06 ns
      </TD>
      <TD>
         17.95 ns
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
         30.58 ns
      </TD>
      <TD CLASS = "thinright">
         30.77 ns
      </TD>
      <TD CLASS = "thinright">
         31.19 ns
      </TD>
      <TD>
         30.54 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         18.70 ns
      </TD>
      <TD CLASS = "thinright">
         18.36 ns
      </TD>
      <TD CLASS = "thinright">
         19.00 ns
      </TD>
      <TD>
         18.51 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         18.49 ns
      </TD>
      <TD CLASS = "thinright">
         18.15 ns
      </TD>
      <TD CLASS = "thinright">
         21.26 ns
      </TD>
      <TD>
         18.14 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         18.01 ns
      </TD>
      <TD CLASS = "thinright">
         17.94 ns
      </TD>
      <TD CLASS = "thinright">
         17.90 ns
      </TD>
      <TD>
         17.90 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:


 There was 12 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 1.72 times faster than Alga
 * Fgl was 1.68 times faster than Alga
 * Containers was 1.67 times faster than Alga

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
         114.0 ns
      </TD>
      <TD CLASS = "thinright">
         2.036 μs
      </TD>
      <TD CLASS = "thinright">
         193.0 μs
      </TD>
      <TD>
         34.34 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         113.4 ns
      </TD>
      <TD CLASS = "thinright">
         3.419 μs
      </TD>
      <TD CLASS = "thinright">
         321.8 μs
      </TD>
      <TD>
         41.05 ms
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
         113.9 ns
      </TD>
      <TD CLASS = "thinright">
         896.2 ns
      </TD>
      <TD CLASS = "thinright">
         9.651 μs
      </TD>
      <TD>
         97.22 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         110.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.408 μs
      </TD>
      <TD CLASS = "thinright">
         18.13 μs
      </TD>
      <TD>
         214.6 μs
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
         1.375 μs
      </TD>
      <TD CLASS = "thinright">
         19.56 μs
      </TD>
      <TD CLASS = "thinright">
         146.4 μs
      </TD>
      <TD>
         1.936 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.169 μs
      </TD>
      <TD CLASS = "thinright">
         33.83 μs
      </TD>
      <TD CLASS = "thinright">
         258.2 μs
      </TD>
      <TD>
         2.849 ms
      </TD>
   </TR>
</TABLE>

Not implemented for Containers because it is a nonsense.
Not implemented for Alga because it is a nonsense.

SUMMARY:

 * Fgl was the fastest 28 times
 * Hash-Graph was the fastest 2 times

 There was 6 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Fgl was 1.53 times faster than Hash-Graph

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
         769.4 ns
      </TD>
      <TD CLASS = "thinright">
         21.87 μs
      </TD>
      <TD CLASS = "thinright">
         3.904 ms
      </TD>
      <TD>
         518.2 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         120.2 ns
      </TD>
      <TD CLASS = "thinright">
         1.094 μs
      </TD>
      <TD CLASS = "thinright">
         83.82 μs
      </TD>
      <TD>
         15.32 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         260.0 ns
      </TD>
      <TD CLASS = "thinright">
         4.841 μs
      </TD>
      <TD CLASS = "thinright">
         329.5 μs
      </TD>
      <TD>
         62.33 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         64.45 ns
      </TD>
      <TD CLASS = "thinright">
         2.910 μs
      </TD>
      <TD CLASS = "thinright">
         287.2 μs
      </TD>
      <TD>
         59.75 ms
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
         776.2 ns
      </TD>
      <TD CLASS = "thinright">
         8.444 μs
      </TD>
      <TD CLASS = "thinright">
         128.1 μs
      </TD>
      <TD>
         2.545 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         119.7 ns
      </TD>
      <TD CLASS = "thinright">
         594.2 ns
      </TD>
      <TD CLASS = "thinright">
         6.015 μs
      </TD>
      <TD>
         62.93 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         261.1 ns
      </TD>
      <TD CLASS = "thinright">
         3.234 μs
      </TD>
      <TD CLASS = "thinright">
         43.54 μs
      </TD>
      <TD>
         734.7 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         64.20 ns
      </TD>
      <TD CLASS = "thinright">
         1.445 μs
      </TD>
      <TD CLASS = "thinright">
         26.26 μs
      </TD>
      <TD>
         883.1 μs
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
         13.25 μs
      </TD>
      <TD CLASS = "thinright">
         358.0 μs
      </TD>
      <TD CLASS = "thinright">
         4.037 ms
      </TD>
      <TD>
         53.31 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         872.5 ns
      </TD>
      <TD CLASS = "thinright">
         18.15 μs
      </TD>
      <TD CLASS = "thinright">
         71.23 μs
      </TD>
      <TD>
         1.355 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         4.958 μs
      </TD>
      <TD CLASS = "thinright">
         153.2 μs
      </TD>
      <TD CLASS = "thinright">
         1.118 ms
      </TD>
      <TD>
         16.03 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.331 μs
      </TD>
      <TD CLASS = "thinright">
         66.86 μs
      </TD>
      <TD CLASS = "thinright">
         491.6 μs
      </TD>
      <TD>
         8.157 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 12 times
 * Hash-Graph was the fastest 10 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 338.06 times faster than Alga
 * Containers was 48.66 times faster than Alga
 * Fgl was 22.59 times faster than Alga

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
         15.64 μs
      </TD>
      <TD CLASS = "thinright">
         7.211 ms
      </TD>
      <TD>
         1.042 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         628.8 ns
      </TD>
      <TD CLASS = "thinright">
         39.29 μs
      </TD>
      <TD>
         23.83 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         4.409 μs
      </TD>
      <TD CLASS = "thinright">
         230.5 μs
      </TD>
      <TD>
         31.09 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         3.950 μs
      </TD>
      <TD CLASS = "thinright">
         370.1 μs
      </TD>
      <TD>
         41.65 ms
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
         4.738 μs
      </TD>
      <TD CLASS = "thinright">
         67.92 μs
      </TD>
      <TD>
         1.718 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         366.7 ns
      </TD>
      <TD CLASS = "thinright">
         2.835 μs
      </TD>
      <TD>
         27.66 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.595 μs
      </TD>
      <TD CLASS = "thinright">
         10.36 μs
      </TD>
      <TD>
         94.87 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1.581 μs
      </TD>
      <TD CLASS = "thinright">
         19.19 μs
      </TD>
      <TD>
         226.6 μs
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
         7.465 μs
      </TD>
      <TD CLASS = "thinright">
         161.5 μs
      </TD>
      <TD CLASS = "thinright">
         5.385 ms
      </TD>
      <TD>
         47.57 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         481.5 ns
      </TD>
      <TD CLASS = "thinright">
         5.237 μs
      </TD>
      <TD CLASS = "thinright">
         31.19 μs
      </TD>
      <TD>
         220.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.955 μs
      </TD>
      <TD CLASS = "thinright">
         24.81 μs
      </TD>
      <TD CLASS = "thinright">
         168.4 μs
      </TD>
      <TD>
         1.344 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.386 μs
      </TD>
      <TD CLASS = "thinright">
         37.06 μs
      </TD>
      <TD CLASS = "thinright">
         282.0 μs
      </TD>
      <TD>
         2.456 ms
      </TD>
   </TR>
</TABLE>


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 68.71 times faster than Alga
 * Fgl was 27.98 times faster than Alga
 * Hash-Graph was 17.97 times faster than Alga

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
         32.13 ns
      </TD>
      <TD CLASS = "thinright">
         3.271 μs
      </TD>
      <TD CLASS = "thinright">
         434.7 μs
      </TD>
      <TD>
         185.6 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         34.32 ns
      </TD>
      <TD CLASS = "thinright">
         774.8 ns
      </TD>
      <TD CLASS = "thinright">
         45.14 μs
      </TD>
      <TD>
         31.03 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         82.62 ns
      </TD>
      <TD CLASS = "thinright">
         2.513 μs
      </TD>
      <TD CLASS = "thinright">
         226.4 μs
      </TD>
      <TD>
         34.19 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         78.36 ns
      </TD>
      <TD CLASS = "thinright">
         6.755 μs
      </TD>
      <TD CLASS = "thinright">
         436.8 μs
      </TD>
      <TD>
         43.90 ms
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
         32.20 ns
      </TD>
      <TD CLASS = "thinright">
         1.016 μs
      </TD>
      <TD CLASS = "thinright">
         13.97 μs
      </TD>
      <TD>
         165.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         35.60 ns
      </TD>
      <TD CLASS = "thinright">
         586.8 ns
      </TD>
      <TD CLASS = "thinright">
         6.812 μs
      </TD>
      <TD>
         69.79 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         84.25 ns
      </TD>
      <TD CLASS = "thinright">
         954.6 ns
      </TD>
      <TD CLASS = "thinright">
         9.853 μs
      </TD>
      <TD>
         96.05 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         78.72 ns
      </TD>
      <TD CLASS = "thinright">
         2.449 μs
      </TD>
      <TD CLASS = "thinright">
         19.95 μs
      </TD>
      <TD>
         219.2 μs
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
         1.615 μs
      </TD>
      <TD CLASS = "thinright">
         29.94 μs
      </TD>
      <TD CLASS = "thinright">
         277.7 μs
      </TD>
      <TD>
         3.561 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         884.0 ns
      </TD>
      <TD CLASS = "thinright">
         8.698 μs
      </TD>
      <TD CLASS = "thinright">
         45.29 μs
      </TD>
      <TD>
         313.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.391 μs
      </TD>
      <TD CLASS = "thinright">
         21.22 μs
      </TD>
      <TD CLASS = "thinright">
         146.7 μs
      </TD>
      <TD>
         1.389 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         3.112 μs
      </TD>
      <TD CLASS = "thinright">
         39.53 μs
      </TD>
      <TD CLASS = "thinright">
         279.6 μs
      </TD>
      <TD>
         2.492 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 26 times
 * Alga was the fastest 1 times
 * Fgl was the fastest 1 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 4.42 times faster than Alga
 * Fgl was 2.77 times faster than Alga
 * Hash-Graph was 1.70 times faster than Alga

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
         595.1 ns
      </TD>
      <TD CLASS = "thinright">
         27.83 μs
      </TD>
      <TD CLASS = "thinright">
         4.820 ms
      </TD>
      <TD>
         590.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         151.7 ns
      </TD>
      <TD CLASS = "thinright">
         2.575 μs
      </TD>
      <TD CLASS = "thinright">
         169.9 μs
      </TD>
      <TD>
         29.00 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         364.8 ns
      </TD>
      <TD CLASS = "thinright">
         9.665 μs
      </TD>
      <TD CLASS = "thinright">
         815.5 μs
      </TD>
      <TD>
         142.0 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         206.9 ns
      </TD>
      <TD CLASS = "thinright">
         6.902 μs
      </TD>
      <TD CLASS = "thinright">
         333.6 μs
      </TD>
      <TD>
         35.96 ms
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
         601.7 ns
      </TD>
      <TD CLASS = "thinright">
         11.20 μs
      </TD>
      <TD CLASS = "thinright">
         170.3 μs
      </TD>
      <TD>
         3.392 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         150.6 ns
      </TD>
      <TD CLASS = "thinright">
         1.573 μs
      </TD>
      <TD CLASS = "thinright">
         15.67 μs
      </TD>
      <TD>
         180.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         371.4 ns
      </TD>
      <TD CLASS = "thinright">
         7.099 μs
      </TD>
      <TD CLASS = "thinright">
         113.5 μs
      </TD>
      <TD>
         2.463 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         206.1 ns
      </TD>
      <TD CLASS = "thinright">
         5.585 μs
      </TD>
      <TD CLASS = "thinright">
         80.88 μs
      </TD>
      <TD>
         1.132 ms
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
         17.55 μs
      </TD>
      <TD CLASS = "thinright">
         366.2 μs
      </TD>
      <TD CLASS = "thinright">
         4.939 ms
      </TD>
      <TD>
         54.15 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         2.457 μs
      </TD>
      <TD CLASS = "thinright">
         24.73 μs
      </TD>
      <TD CLASS = "thinright">
         171.2 μs
      </TD>
      <TD>
         4.128 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         11.37 μs
      </TD>
      <TD CLASS = "thinright">
         189.8 μs
      </TD>
      <TD CLASS = "thinright">
         3.075 ms
      </TD>
      <TD>
         26.39 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         8.958 μs
      </TD>
      <TD CLASS = "thinright">
         28.32 μs
      </TD>
      <TD CLASS = "thinright">
         76.62 μs
      </TD>
      <TD>
         497.9 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 51.86 times faster than Alga
 * Containers was 16.81 times faster than Alga
 * Fgl was 2.43 times faster than Alga


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
         25.55 ns
      </TD>
      <TD CLASS = "thinright">
         3.103 μs
      </TD>
      <TD CLASS = "thinright">
         332.1 μs
      </TD>
      <TD>
         34.05 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         87.48 ns
      </TD>
      <TD CLASS = "thinright">
         2.092 μs
      </TD>
      <TD CLASS = "thinright">
         270.2 μs
      </TD>
      <TD>
         87.50 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         121.3 ns
      </TD>
      <TD CLASS = "thinright">
         13.60 μs
      </TD>
      <TD CLASS = "thinright">
         2.552 ms
      </TD>
      <TD>
         415.0 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         52.01 ns
      </TD>
      <TD CLASS = "thinright">
         13.77 μs
      </TD>
      <TD CLASS = "thinright">
         4.095 ms
      </TD>
      <TD>
         740.4 ms
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
         26.32 ns
      </TD>
      <TD CLASS = "thinright">
         902.8 ns
      </TD>
      <TD CLASS = "thinright">
         12.38 μs
      </TD>
      <TD>
         130.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         87.29 ns
      </TD>
      <TD CLASS = "thinright">
         885.1 ns
      </TD>
      <TD CLASS = "thinright">
         9.721 μs
      </TD>
      <TD>
         115.7 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         115.8 ns
      </TD>
      <TD CLASS = "thinright">
         4.330 μs
      </TD>
      <TD CLASS = "thinright">
         56.55 μs
      </TD>
      <TD>
         731.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         50.45 ns
      </TD>
      <TD CLASS = "thinright">
         2.958 μs
      </TD>
      <TD CLASS = "thinright">
         51.49 μs
      </TD>
      <TD>
         1.154 ms
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
         1.450 μs
      </TD>
      <TD CLASS = "thinright">
         26.13 μs
      </TD>
      <TD CLASS = "thinright">
         250.7 μs
      </TD>
      <TD>
         2.061 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         1.347 μs
      </TD>
      <TD CLASS = "thinright">
         21.21 μs
      </TD>
      <TD CLASS = "thinright">
         195.3 μs
      </TD>
      <TD>
         3.898 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         7.178 μs
      </TD>
      <TD CLASS = "thinright">
         126.2 μs
      </TD>
      <TD CLASS = "thinright">
         1.824 ms
      </TD>
      <TD>
         17.97 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         4.816 μs
      </TD>
      <TD CLASS = "thinright">
         143.2 μs
      </TD>
      <TD CLASS = "thinright">
         3.048 ms
      </TD>
      <TD>
         25.97 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 6 times
 * Alga was the fastest 4 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 14.52 times faster than Hash-Graph
 * Containers was 8.14 times faster than Hash-Graph
 * Fgl was 1.59 times faster than Hash-Graph

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
         30.70 ns
      </TD>
      <TD CLASS = "thinright">
         1.641 μs
      </TD>
      <TD CLASS = "thinright">
         333.0 μs
      </TD>
      <TD>
         78.40 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         20.68 ns
      </TD>
      <TD CLASS = "thinright">
         21.08 ns
      </TD>
      <TD CLASS = "thinright">
         20.70 ns
      </TD>
      <TD>
         21.08 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         20.91 ns
      </TD>
      <TD CLASS = "thinright">
         100.4 ns
      </TD>
      <TD CLASS = "thinright">
         939.2 ns
      </TD>
      <TD>
         10.12 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         22.34 ns
      </TD>
      <TD CLASS = "thinright">
         88.78 ns
      </TD>
      <TD CLASS = "thinright">
         783.7 ns
      </TD>
      <TD>
         8.315 μs
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
         31.27 ns
      </TD>
      <TD CLASS = "thinright">
         536.3 ns
      </TD>
      <TD CLASS = "thinright">
         9.404 μs
      </TD>
      <TD>
         145.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         21.33 ns
      </TD>
      <TD CLASS = "thinright">
         20.89 ns
      </TD>
      <TD CLASS = "thinright">
         21.57 ns
      </TD>
      <TD>
         20.93 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         21.27 ns
      </TD>
      <TD CLASS = "thinright">
         101.8 ns
      </TD>
      <TD CLASS = "thinright">
         902.7 ns
      </TD>
      <TD>
         9.976 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         22.51 ns
      </TD>
      <TD CLASS = "thinright">
         83.41 ns
      </TD>
      <TD CLASS = "thinright">
         792.4 ns
      </TD>
      <TD>
         8.361 μs
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
         989.4 ns
      </TD>
      <TD CLASS = "thinright">
         27.31 μs
      </TD>
      <TD CLASS = "thinright">
         419.4 μs
      </TD>
      <TD>
         6.483 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         20.85 ns
      </TD>
      <TD CLASS = "thinright">
         22.58 ns
      </TD>
      <TD CLASS = "thinright">
         20.94 ns
      </TD>
      <TD>
         21.19 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         132.8 ns
      </TD>
      <TD CLASS = "thinright">
         733.1 ns
      </TD>
      <TD CLASS = "thinright">
         3.387 μs
      </TD>
      <TD>
         16.86 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         117.2 ns
      </TD>
      <TD CLASS = "thinright">
         700.9 ns
      </TD>
      <TD CLASS = "thinright">
         3.330 μs
      </TD>
      <TD>
         13.14 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 20395.10 times faster than Alga
 * Hash-Graph was 45.21 times faster than Alga
 * Fgl was 36.97 times faster than Alga


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
         41.16 ns
      </TD>
      <TD CLASS = "thinright">
         2.182 μs
      </TD>
      <TD CLASS = "thinright">
         334.0 μs
      </TD>
      <TD>
         77.93 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         31.64 ns
      </TD>
      <TD CLASS = "thinright">
         151.8 ns
      </TD>
      <TD CLASS = "thinright">
         1.044 μs
      </TD>
      <TD>
         10.23 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         49.90 ns
      </TD>
      <TD CLASS = "thinright">
         416.8 ns
      </TD>
      <TD CLASS = "thinright">
         4.158 μs
      </TD>
      <TD>
         42.37 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         23.04 ns
      </TD>
      <TD CLASS = "thinright">
         200.6 ns
      </TD>
      <TD CLASS = "thinright">
         2.224 μs
      </TD>
      <TD>
         26.18 μs
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
         40.90 ns
      </TD>
      <TD CLASS = "thinright">
         724.2 ns
      </TD>
      <TD CLASS = "thinright">
         10.21 μs
      </TD>
      <TD>
         153.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         32.02 ns
      </TD>
      <TD CLASS = "thinright">
         148.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.047 μs
      </TD>
      <TD>
         10.34 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         50.35 ns
      </TD>
      <TD CLASS = "thinright">
         413.9 ns
      </TD>
      <TD CLASS = "thinright">
         4.175 μs
      </TD>
      <TD>
         42.46 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         22.92 ns
      </TD>
      <TD CLASS = "thinright">
         201.1 ns
      </TD>
      <TD CLASS = "thinright">
         2.225 μs
      </TD>
      <TD>
         26.16 μs
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
         1.133 μs
      </TD>
      <TD CLASS = "thinright">
         28.39 μs
      </TD>
      <TD CLASS = "thinright">
         419.0 μs
      </TD>
      <TD>
         7.368 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         189.2 ns
      </TD>
      <TD CLASS = "thinright">
         916.3 ns
      </TD>
      <TD CLASS = "thinright">
         3.664 μs
      </TD>
      <TD>
         17.00 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         606.5 ns
      </TD>
      <TD CLASS = "thinright">
         3.654 μs
      </TD>
      <TD CLASS = "thinright">
         14.88 μs
      </TD>
      <TD>
         71.00 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         278.4 ns
      </TD>
      <TD CLASS = "thinright">
         2.005 μs
      </TD>
      <TD CLASS = "thinright">
         9.463 μs
      </TD>
      <TD>
         36.20 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 42.88 times faster than Alga
 * Hash-Graph was 17.94 times faster than Alga
 * Fgl was 10.35 times faster than Alga
