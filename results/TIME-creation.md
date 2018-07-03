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
         51.86 ns
      </TD>
      <TD CLASS = "thinright">
         42.86 μs
      </TD>
      <TD CLASS = "thinright">
         9.489 ms
      </TD>
      <TD>
         1.345 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         42.18 ns
      </TD>
      <TD CLASS = "thinright">
         1.774 μs
      </TD>
      <TD CLASS = "thinright">
         207.9 μs
      </TD>
      <TD>
         72.91 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         39.36 ns
      </TD>
      <TD CLASS = "thinright">
         18.93 μs
      </TD>
      <TD CLASS = "thinright">
         9.533 ms
      </TD>
      <TD>
         3.436 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         40.19 ns
      </TD>
      <TD CLASS = "thinright">
         21.98 μs
      </TD>
      <TD CLASS = "thinright">
         7.785 ms
      </TD>
      <TD>
         3.975 s
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
         52.01 ns
      </TD>
      <TD CLASS = "thinright">
         11.62 μs
      </TD>
      <TD CLASS = "thinright">
         222.0 μs
      </TD>
      <TD>
         4.745 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         41.96 ns
      </TD>
      <TD CLASS = "thinright">
         710.5 ns
      </TD>
      <TD CLASS = "thinright">
         9.064 μs
      </TD>
      <TD>
         95.95 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         37.11 ns
      </TD>
      <TD CLASS = "thinright">
         6.165 μs
      </TD>
      <TD CLASS = "thinright">
         216.5 μs
      </TD>
      <TD>
         23.73 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         42.36 ns
      </TD>
      <TD CLASS = "thinright">
         5.970 μs
      </TD>
      <TD CLASS = "thinright">
         203.9 μs
      </TD>
      <TD>
         19.92 ms
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
         20.34 μs
      </TD>
      <TD CLASS = "thinright">
         600.5 μs
      </TD>
      <TD CLASS = "thinright">
         7.619 ms
      </TD>
      <TD>
         91.10 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         1.025 μs
      </TD>
      <TD CLASS = "thinright">
         17.98 μs
      </TD>
      <TD CLASS = "thinright">
         158.7 μs
      </TD>
      <TD>
         3.092 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         11.06 μs
      </TD>
      <TD CLASS = "thinright">
         533.7 μs
      </TD>
      <TD CLASS = "thinright">
         19.24 ms
      </TD>
      <TD>
         576.6 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         10.96 μs
      </TD>
      <TD CLASS = "thinright">
         518.7 μs
      </TD>
      <TD CLASS = "thinright">
         18.84 ms
      </TD>
      <TD>
         598.4 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Fgl was the fastest 1 times

 There was 1 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 105.90 times faster than Hash-Graph
 * Alga was 3.83 times faster than Hash-Graph
 * Fgl was 1.08 times faster than Hash-Graph

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
         48.49 ns
      </TD>
      <TD CLASS = "thinright">
         20.52 μs
      </TD>
      <TD CLASS = "thinright">
         5.130 ms
      </TD>
      <TD>
         856.5 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         35.52 ns
      </TD>
      <TD CLASS = "thinright">
         960.3 ns
      </TD>
      <TD CLASS = "thinright">
         105.1 μs
      </TD>
      <TD>
         81.59 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         63.03 ns
      </TD>
      <TD CLASS = "thinright">
         15.08 μs
      </TD>
      <TD CLASS = "thinright">
         10.09 ms
      </TD>
      <TD>
         3.311 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         27.74 ns
      </TD>
      <TD CLASS = "thinright">
         19.85 μs
      </TD>
      <TD CLASS = "thinright">
         10.36 ms
      </TD>
      <TD>
         3.804 s
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
         48.75 ns
      </TD>
      <TD CLASS = "thinright">
         5.687 μs
      </TD>
      <TD CLASS = "thinright">
         141.3 μs
      </TD>
      <TD>
         2.394 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         35.56 ns
      </TD>
      <TD CLASS = "thinright">
         404.3 ns
      </TD>
      <TD CLASS = "thinright">
         5.359 μs
      </TD>
      <TD>
         44.82 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         75.73 ns
      </TD>
      <TD CLASS = "thinright">
         5.686 μs
      </TD>
      <TD CLASS = "thinright">
         202.3 μs
      </TD>
      <TD>
         18.27 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         31.99 ns
      </TD>
      <TD CLASS = "thinright">
         5.249 μs
      </TD>
      <TD CLASS = "thinright">
         196.2 μs
      </TD>
      <TD>
         21.46 ms
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
         9.681 μs
      </TD>
      <TD CLASS = "thinright">
         287.1 μs
      </TD>
      <TD CLASS = "thinright">
         3.387 ms
      </TD>
      <TD>
         40.55 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         560.3 ns
      </TD>
      <TD CLASS = "thinright">
         9.040 μs
      </TD>
      <TD CLASS = "thinright">
         77.43 μs
      </TD>
      <TD>
         992.7 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         9.476 μs
      </TD>
      <TD CLASS = "thinright">
         471.9 μs
      </TD>
      <TD CLASS = "thinright">
         17.04 ms
      </TD>
      <TD>
         516.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         9.489 μs
      </TD>
      <TD CLASS = "thinright">
         490.6 μs
      </TD>
      <TD CLASS = "thinright">
         18.27 ms
      </TD>
      <TD>
         580.3 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 118.84 times faster than Hash-Graph
 * Alga was 5.48 times faster than Hash-Graph
 * Fgl was 1.15 times faster than Hash-Graph

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
         194.6 ns
      </TD>
      <TD CLASS = "thinright">
         63.87 μs
      </TD>
      <TD CLASS = "thinright">
         13.65 ms
      </TD>
      <TD>
         1.947 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         33.27 ns
      </TD>
      <TD CLASS = "thinright">
         1.059 μs
      </TD>
      <TD CLASS = "thinright">
         122.3 μs
      </TD>
      <TD>
         89.36 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         167.8 ns
      </TD>
      <TD CLASS = "thinright">
         30.49 μs
      </TD>
      <TD CLASS = "thinright">
         10.93 ms
      </TD>
      <TD>
         3.750 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         70.81 ns
      </TD>
      <TD CLASS = "thinright">
         24.83 μs
      </TD>
      <TD CLASS = "thinright">
         8.072 ms
      </TD>
      <TD>
         3.674 s
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
         195.3 ns
      </TD>
      <TD CLASS = "thinright">
         17.79 μs
      </TD>
      <TD CLASS = "thinright">
         331.6 μs
      </TD>
      <TD>
         6.538 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         33.05 ns
      </TD>
      <TD CLASS = "thinright">
         380.4 ns
      </TD>
      <TD CLASS = "thinright">
         5.487 μs
      </TD>
      <TD>
         45.86 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         167.0 ns
      </TD>
      <TD CLASS = "thinright">
         10.79 μs
      </TD>
      <TD CLASS = "thinright">
         265.4 μs
      </TD>
      <TD>
         18.96 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         71.23 ns
      </TD>
      <TD CLASS = "thinright">
         6.916 μs
      </TD>
      <TD CLASS = "thinright">
         221.6 μs
      </TD>
      <TD>
         18.85 ms
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
         29.44 μs
      </TD>
      <TD CLASS = "thinright">
         844.2 μs
      </TD>
      <TD CLASS = "thinright">
         11.97 ms
      </TD>
      <TD>
         139.5 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         545.6 ns
      </TD>
      <TD CLASS = "thinright">
         10.60 μs
      </TD>
      <TD CLASS = "thinright">
         93.95 μs
      </TD>
      <TD>
         1.158 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         17.28 μs
      </TD>
      <TD CLASS = "thinright">
         618.2 μs
      </TD>
      <TD CLASS = "thinright">
         20.38 ms
      </TD>
      <TD>
         556.7 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         12.08 μs
      </TD>
      <TD CLASS = "thinright">
         541.6 μs
      </TD>
      <TD CLASS = "thinright">
         17.54 ms
      </TD>
      <TD>
         556.1 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 22 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 104.51 times faster than Fgl
 * Alga was 2.28 times faster than Fgl
 * Hash-Graph was 1.00 times faster than Fgl

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
         28.75 ns
      </TD>
      <TD CLASS = "thinright">
         5.666 μs
      </TD>
      <TD CLASS = "thinright">
         588.6 μs
      </TD>
      <TD>
         64.89 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         92.80 ns
      </TD>
      <TD CLASS = "thinright">
         2.969 μs
      </TD>
      <TD CLASS = "thinright">
         497.0 μs
      </TD>
      <TD>
         141.7 ms
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
         29.42 ns
      </TD>
      <TD CLASS = "thinright">
         1.670 μs
      </TD>
      <TD CLASS = "thinright">
         21.25 μs
      </TD>
      <TD>
         229.7 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         100.2 ns
      </TD>
      <TD CLASS = "thinright">
         1.199 μs
      </TD>
      <TD CLASS = "thinright">
         14.50 μs
      </TD>
      <TD>
         197.1 μs
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
         2.718 μs
      </TD>
      <TD CLASS = "thinright">
         49.05 μs
      </TD>
      <TD CLASS = "thinright">
         382.2 μs
      </TD>
      <TD>
         3.210 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         1.730 μs
      </TD>
      <TD CLASS = "thinright">
         30.11 μs
      </TD>
      <TD CLASS = "thinright">
         313.8 μs
      </TD>
      <TD>
         7.580 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 8 times
 * Alga was the fastest 4 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 1.80 times faster than Containers

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
         Containers
      </TH>
      <TD CLASS = "thinright">
         157.5 ns
      </TD>
      <TD CLASS = "thinright">
         3.417 μs
      </TD>
      <TD CLASS = "thinright">
         341.3 μs
      </TD>
      <TD>
         89.81 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         227.0 ns
      </TD>
      <TD CLASS = "thinright">
         24.58 μs
      </TD>
      <TD CLASS = "thinright">
         11.87 ms
      </TD>
      <TD>
         3.750 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         67.98 ns
      </TD>
      <TD CLASS = "thinright">
         26.08 μs
      </TD>
      <TD CLASS = "thinright">
         7.971 ms
      </TD>
      <TD>
         4.195 s
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
         159.4 ns
      </TD>
      <TD CLASS = "thinright">
         1.764 μs
      </TD>
      <TD CLASS = "thinright">
         19.81 μs
      </TD>
      <TD>
         253.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         229.3 ns
      </TD>
      <TD CLASS = "thinright">
         11.36 μs
      </TD>
      <TD CLASS = "thinright">
         303.7 μs
      </TD>
      <TD>
         19.77 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         68.49 ns
      </TD>
      <TD CLASS = "thinright">
         8.093 μs
      </TD>
      <TD CLASS = "thinright">
         244.8 μs
      </TD>
      <TD>
         20.44 ms
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
         2.613 μs
      </TD>
      <TD CLASS = "thinright">
         32.19 μs
      </TD>
      <TD CLASS = "thinright">
         312.1 μs
      </TD>
      <TD>
         8.453 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         18.88 μs
      </TD>
      <TD CLASS = "thinright">
         688.4 μs
      </TD>
      <TD CLASS = "thinright">
         22.33 ms
      </TD>
      <TD>
         582.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         14.11 μs
      </TD>
      <TD CLASS = "thinright">
         568.1 μs
      </TD>
      <TD CLASS = "thinright">
         17.45 ms
      </TD>
      <TD>
         554.7 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 61.17 times faster than Hash-Graph
 * Fgl was 1.05 times faster than Hash-Graph

## topSort

Description: Topological sorting of the vertices

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
         141.5 ns
      </TD>
      <TD CLASS = "thinright">
         1.807 μs
      </TD>
      <TD CLASS = "thinright">
         19.53 μs
      </TD>
      <TD>
         254.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         359.7 ns
      </TD>
      <TD CLASS = "thinright">
         14.27 μs
      </TD>
      <TD CLASS = "thinright">
         339.8 μs
      </TD>
      <TD>
         22.77 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         209.2 ns
      </TD>
      <TD CLASS = "thinright">
         11.43 μs
      </TD>
      <TD CLASS = "thinright">
         278.7 μs
      </TD>
      <TD>
         19.63 ms
      </TD>
   </TR>
</TABLE>

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
         143.9 ns
      </TD>
      <TD CLASS = "thinright">
         3.579 μs
      </TD>
      <TD CLASS = "thinright">
         364.7 μs
      </TD>
      <TD>
         100.6 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         361.9 ns
      </TD>
      <TD CLASS = "thinright">
         27.78 μs
      </TD>
      <TD CLASS = "thinright">
         10.49 ms
      </TD>
      <TD>
         3.615 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         208.2 ns
      </TD>
      <TD CLASS = "thinright">
         26.53 μs
      </TD>
      <TD CLASS = "thinright">
         7.743 ms
      </TD>
      <TD>
         3.486 s
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
         2.751 μs
      </TD>
      <TD CLASS = "thinright">
         32.74 μs
      </TD>
      <TD CLASS = "thinright">
         284.4 μs
      </TD>
      <TD>
         6.213 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         23.22 μs
      </TD>
      <TD CLASS = "thinright">
         753.4 μs
      </TD>
      <TD CLASS = "thinright">
         22.99 ms
      </TD>
      <TD>
         611.3 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         19.17 μs
      </TD>
      <TD CLASS = "thinright">
         525.1 μs
      </TD>
      <TD CLASS = "thinright">
         17.83 ms
      </TD>
      <TD>
         557.1 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 12 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 61.01 times faster than Fgl
 * Hash-Graph was 1.08 times faster than Fgl


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
         128.4 ns
      </TD>
      <TD CLASS = "thinright">
         1.941 μs
      </TD>
      <TD CLASS = "thinright">
         216.8 μs
      </TD>
      <TD>
         79.34 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         271.2 ns
      </TD>
      <TD CLASS = "thinright">
         21.96 μs
      </TD>
      <TD CLASS = "thinright">
         8.321 ms
      </TD>
      <TD>
         3.304 s
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
         125.1 ns
      </TD>
      <TD CLASS = "thinright">
         882.6 ns
      </TD>
      <TD CLASS = "thinright">
         9.750 μs
      </TD>
      <TD>
         109.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         277.6 ns
      </TD>
      <TD CLASS = "thinright">
         8.911 μs
      </TD>
      <TD CLASS = "thinright">
         240.8 μs
      </TD>
      <TD>
         19.31 ms
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
         1.270 μs
      </TD>
      <TD CLASS = "thinright">
         26.51 μs
      </TD>
      <TD CLASS = "thinright">
         160.7 μs
      </TD>
      <TD>
         2.907 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         14.52 μs
      </TD>
      <TD CLASS = "thinright">
         677.0 μs
      </TD>
      <TD CLASS = "thinright">
         19.47 ms
      </TD>
      <TD>
         581.3 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 22 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 87.99 times faster than Fgl

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
         58.76 ns
      </TD>
      <TD CLASS = "thinright">
         42.95 μs
      </TD>
      <TD CLASS = "thinright">
         9.209 ms
      </TD>
      <TD>
         1.298 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         32.66 ns
      </TD>
      <TD CLASS = "thinright">
         1.066 μs
      </TD>
      <TD CLASS = "thinright">
         104.8 μs
      </TD>
      <TD>
         80.94 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         37.68 ns
      </TD>
      <TD CLASS = "thinright">
         17.73 μs
      </TD>
      <TD CLASS = "thinright">
         9.106 ms
      </TD>
      <TD>
         3.357 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         40.56 ns
      </TD>
      <TD CLASS = "thinright">
         20.79 μs
      </TD>
      <TD CLASS = "thinright">
         7.612 ms
      </TD>
      <TD>
         3.634 s
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
         57.94 ns
      </TD>
      <TD CLASS = "thinright">
         11.51 μs
      </TD>
      <TD CLASS = "thinright">
         220.5 μs
      </TD>
      <TD>
         3.924 ms
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
         407.3 ns
      </TD>
      <TD CLASS = "thinright">
         5.049 μs
      </TD>
      <TD>
         46.84 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         39.78 ns
      </TD>
      <TD CLASS = "thinright">
         6.008 μs
      </TD>
      <TD CLASS = "thinright">
         213.6 μs
      </TD>
      <TD>
         18.61 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         42.04 ns
      </TD>
      <TD CLASS = "thinright">
         6.019 μs
      </TD>
      <TD CLASS = "thinright">
         208.9 μs
      </TD>
      <TD>
         19.14 ms
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
         19.86 μs
      </TD>
      <TD CLASS = "thinright">
         580.0 μs
      </TD>
      <TD CLASS = "thinright">
         6.794 ms
      </TD>
      <TD>
         94.76 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         585.3 ns
      </TD>
      <TD CLASS = "thinright">
         10.80 μs
      </TD>
      <TD CLASS = "thinright">
         99.63 μs
      </TD>
      <TD>
         1.156 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         10.22 μs
      </TD>
      <TD CLASS = "thinright">
         513.6 μs
      </TD>
      <TD CLASS = "thinright">
         19.26 ms
      </TD>
      <TD>
         587.0 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         10.16 μs
      </TD>
      <TD CLASS = "thinright">
         494.3 μs
      </TD>
      <TD CLASS = "thinright">
         17.77 ms
      </TD>
      <TD>
         541.0 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 12 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 111.69 times faster than Hash-Graph
 * Alga was 3.22 times faster than Hash-Graph
 * Fgl was 1.06 times faster than Hash-Graph

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
         696.7 ns
      </TD>
      <TD CLASS = "thinright">
         12.76 μs
      </TD>
      <TD CLASS = "thinright">
         1.077 ms
      </TD>
      <TD>
         274.3 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         30.63 ns
      </TD>
      <TD CLASS = "thinright">
         900.9 ns
      </TD>
      <TD CLASS = "thinright">
         87.69 μs
      </TD>
      <TD>
         74.16 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         120.8 ns
      </TD>
      <TD CLASS = "thinright">
         18.16 μs
      </TD>
      <TD CLASS = "thinright">
         8.749 ms
      </TD>
      <TD>
         3.322 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         107.1 ns
      </TD>
      <TD CLASS = "thinright">
         19.69 μs
      </TD>
      <TD CLASS = "thinright">
         7.439 ms
      </TD>
      <TD>
         3.587 s
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
         718.7 ns
      </TD>
      <TD CLASS = "thinright">
         4.997 μs
      </TD>
      <TD CLASS = "thinright">
         39.47 μs
      </TD>
      <TD>
         420.6 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         31.28 ns
      </TD>
      <TD CLASS = "thinright">
         321.9 ns
      </TD>
      <TD CLASS = "thinright">
         3.696 μs
      </TD>
      <TD>
         44.70 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         121.6 ns
      </TD>
      <TD CLASS = "thinright">
         5.550 μs
      </TD>
      <TD CLASS = "thinright">
         194.1 μs
      </TD>
      <TD>
         18.14 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         108.3 ns
      </TD>
      <TD CLASS = "thinright">
         5.288 μs
      </TD>
      <TD CLASS = "thinright">
         194.5 μs
      </TD>
      <TD>
         19.43 ms
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
         6.451 μs
      </TD>
      <TD CLASS = "thinright">
         87.08 μs
      </TD>
      <TD CLASS = "thinright">
         683.4 μs
      </TD>
      <TD>
         11.32 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         412.2 ns
      </TD>
      <TD CLASS = "thinright">
         7.962 μs
      </TD>
      <TD CLASS = "thinright">
         72.64 μs
      </TD>
      <TD>
         892.6 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         9.231 μs
      </TD>
      <TD CLASS = "thinright">
         452.4 μs
      </TD>
      <TD CLASS = "thinright">
         16.12 ms
      </TD>
      <TD>
         519.9 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         9.639 μs
      </TD>
      <TD CLASS = "thinright">
         488.2 μs
      </TD>
      <TD CLASS = "thinright">
         19.32 ms
      </TD>
      <TD>
         539.9 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 62 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 121.86 times faster than Hash-Graph
 * Alga was 18.50 times faster than Hash-Graph
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
         23.62 ns
      </TD>
      <TD CLASS = "thinright">
         73.74 ns
      </TD>
      <TD CLASS = "thinright">
         75.52 ns
      </TD>
      <TD>
         79.23 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         22.74 ns
      </TD>
      <TD CLASS = "thinright">
         12.26 μs
      </TD>
      <TD CLASS = "thinright">
         5.544 ms
      </TD>
      <TD>
         2.652 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         22.38 ns
      </TD>
      <TD CLASS = "thinright">
         19.48 μs
      </TD>
      <TD CLASS = "thinright">
         8.421 ms
      </TD>
      <TD>
         3.602 s
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
         23.59 ns
      </TD>
      <TD CLASS = "thinright">
         78.80 ns
      </TD>
      <TD CLASS = "thinright">
         75.80 ns
      </TD>
      <TD>
         81.41 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         22.47 ns
      </TD>
      <TD CLASS = "thinright">
         4.567 μs
      </TD>
      <TD CLASS = "thinright">
         190.5 μs
      </TD>
      <TD>
         18.15 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         21.74 ns
      </TD>
      <TD CLASS = "thinright">
         5.020 μs
      </TD>
      <TD CLASS = "thinright">
         195.1 μs
      </TD>
      <TD>
         18.58 ms
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
         77.95 ns
      </TD>
      <TD CLASS = "thinright">
         83.41 ns
      </TD>
      <TD CLASS = "thinright">
         75.01 ns
      </TD>
      <TD>
         75.08 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         7.962 μs
      </TD>
      <TD CLASS = "thinright">
         442.6 μs
      </TD>
      <TD CLASS = "thinright">
         15.81 ms
      </TD>
      <TD>
         528.3 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         9.400 μs
      </TD>
      <TD CLASS = "thinright">
         490.3 μs
      </TD>
      <TD CLASS = "thinright">
         17.49 ms
      </TD>
      <TD>
         544.4 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 10 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 660643.95 times faster than Hash-Graph
 * Fgl was 1.03 times faster than Hash-Graph

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
         35.49 ns
      </TD>
      <TD CLASS = "thinright">
         19.73 μs
      </TD>
      <TD CLASS = "thinright">
         4.777 ms
      </TD>
      <TD>
         859.9 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         26.27 ns
      </TD>
      <TD CLASS = "thinright">
         12.50 μs
      </TD>
      <TD CLASS = "thinright">
         4.693 ms
      </TD>
      <TD>
         2.799 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         27.14 ns
      </TD>
      <TD CLASS = "thinright">
         19.88 μs
      </TD>
      <TD CLASS = "thinright">
         7.467 ms
      </TD>
      <TD>
         4.050 s
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
         34.27 ns
      </TD>
      <TD CLASS = "thinright">
         5.608 μs
      </TD>
      <TD CLASS = "thinright">
         131.4 μs
      </TD>
      <TD>
         2.274 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         25.93 ns
      </TD>
      <TD CLASS = "thinright">
         4.522 μs
      </TD>
      <TD CLASS = "thinright">
         186.6 μs
      </TD>
      <TD>
         17.95 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         26.91 ns
      </TD>
      <TD CLASS = "thinright">
         5.132 μs
      </TD>
      <TD CLASS = "thinright">
         190.7 μs
      </TD>
      <TD>
         18.21 ms
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
         9.614 μs
      </TD>
      <TD CLASS = "thinright">
         301.3 μs
      </TD>
      <TD CLASS = "thinright">
         3.361 ms
      </TD>
      <TD>
         47.35 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         8.147 μs
      </TD>
      <TD CLASS = "thinright">
         446.2 μs
      </TD>
      <TD CLASS = "thinright">
         16.35 ms
      </TD>
      <TD>
         526.1 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         10.79 μs
      </TD>
      <TD CLASS = "thinright">
         498.1 μs
      </TD>
      <TD CLASS = "thinright">
         17.80 ms
      </TD>
      <TD>
         548.2 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 6 times
 * Fgl was the fastest 3 times

 There was 3 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 7.08 times faster than Hash-Graph
 * Fgl was 1.24 times faster than Hash-Graph

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
         36.44 ns
      </TD>
      <TD CLASS = "thinright">
         1.319 μs
      </TD>
      <TD CLASS = "thinright">
         116.2 μs
      </TD>
      <TD>
         12.50 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         62.42 ns
      </TD>
      <TD CLASS = "thinright">
         16.39 μs
      </TD>
      <TD CLASS = "thinright">
         8.972 ms
      </TD>
      <TD>
         3.250 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         28.02 ns
      </TD>
      <TD CLASS = "thinright">
         19.60 μs
      </TD>
      <TD CLASS = "thinright">
         8.924 ms
      </TD>
      <TD>
         3.471 s
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
         34.33 ns
      </TD>
      <TD CLASS = "thinright">
         615.8 ns
      </TD>
      <TD CLASS = "thinright">
         7.875 μs
      </TD>
      <TD>
         88.56 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         55.02 ns
      </TD>
      <TD CLASS = "thinright">
         4.956 μs
      </TD>
      <TD CLASS = "thinright">
         190.6 μs
      </TD>
      <TD>
         17.96 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         28.11 ns
      </TD>
      <TD CLASS = "thinright">
         5.023 μs
      </TD>
      <TD CLASS = "thinright">
         194.1 μs
      </TD>
      <TD>
         18.52 ms
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
         944.1 ns
      </TD>
      <TD CLASS = "thinright">
         13.35 μs
      </TD>
      <TD CLASS = "thinright">
         144.6 μs
      </TD>
      <TD>
         700.6 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         8.636 μs
      </TD>
      <TD CLASS = "thinright">
         452.4 μs
      </TD>
      <TD CLASS = "thinright">
         15.79 ms
      </TD>
      <TD>
         523.3 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         9.210 μs
      </TD>
      <TD CLASS = "thinright">
         489.9 μs
      </TD>
      <TD CLASS = "thinright">
         17.33 ms
      </TD>
      <TD>
         544.2 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 40 times
 * Hash-Graph was the fastest 2 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 304.69 times faster than Hash-Graph
 * Fgl was 1.10 times faster than Hash-Graph

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
         50.69 ns
      </TD>
      <TD CLASS = "thinright">
         3.691 μs
      </TD>
      <TD CLASS = "thinright">
         400.5 μs
      </TD>
      <TD>
         44.97 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         123.1 ns
      </TD>
      <TD CLASS = "thinright">
         19.97 μs
      </TD>
      <TD CLASS = "thinright">
         10.34 ms
      </TD>
      <TD>
         3.573 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         172.3 ns
      </TD>
      <TD CLASS = "thinright">
         25.47 μs
      </TD>
      <TD CLASS = "thinright">
         8.626 ms
      </TD>
      <TD>
         3.797 s
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
         49.85 ns
      </TD>
      <TD CLASS = "thinright">
         1.130 μs
      </TD>
      <TD CLASS = "thinright">
         14.60 μs
      </TD>
      <TD>
         160.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         121.9 ns
      </TD>
      <TD CLASS = "thinright">
         6.304 μs
      </TD>
      <TD CLASS = "thinright">
         208.7 μs
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
         171.7 ns
      </TD>
      <TD CLASS = "thinright">
         7.468 μs
      </TD>
      <TD CLASS = "thinright">
         219.1 μs
      </TD>
      <TD>
         19.15 ms
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
         1.779 μs
      </TD>
      <TD CLASS = "thinright">
         34.26 μs
      </TD>
      <TD CLASS = "thinright">
         271.9 μs
      </TD>
      <TD>
         2.105 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         10.70 μs
      </TD>
      <TD CLASS = "thinright">
         522.0 μs
      </TD>
      <TD CLASS = "thinright">
         20.16 ms
      </TD>
      <TD>
         582.8 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         12.79 μs
      </TD>
      <TD CLASS = "thinright">
         525.2 μs
      </TD>
      <TD CLASS = "thinright">
         19.01 ms
      </TD>
      <TD>
         579.8 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 42 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 125.75 times faster than Hash-Graph
 * Fgl was 1.03 times faster than Hash-Graph

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
         38.54 ns
      </TD>
      <TD CLASS = "thinright">
         3.666 μs
      </TD>
      <TD CLASS = "thinright">
         391.3 μs
      </TD>
      <TD>
         40.42 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         77.54 ns
      </TD>
      <TD CLASS = "thinright">
         18.82 μs
      </TD>
      <TD CLASS = "thinright">
         12.60 ms
      </TD>
      <TD>
         3.536 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         108.0 ns
      </TD>
      <TD CLASS = "thinright">
         24.70 μs
      </TD>
      <TD CLASS = "thinright">
         8.503 ms
      </TD>
      <TD>
         3.511 s
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
         39.23 ns
      </TD>
      <TD CLASS = "thinright">
         1.137 μs
      </TD>
      <TD CLASS = "thinright">
         14.87 μs
      </TD>
      <TD>
         157.9 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         77.19 ns
      </TD>
      <TD CLASS = "thinright">
         6.036 μs
      </TD>
      <TD CLASS = "thinright">
         207.2 μs
      </TD>
      <TD>
         19.13 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         106.2 ns
      </TD>
      <TD CLASS = "thinright">
         7.242 μs
      </TD>
      <TD CLASS = "thinright">
         217.6 μs
      </TD>
      <TD>
         19.46 ms
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
         1.739 μs
      </TD>
      <TD CLASS = "thinright">
         32.48 μs
      </TD>
      <TD CLASS = "thinright">
         256.7 μs
      </TD>
      <TD>
         2.079 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         10.84 μs
      </TD>
      <TD CLASS = "thinright">
         524.4 μs
      </TD>
      <TD CLASS = "thinright">
         19.86 ms
      </TD>
      <TD>
         599.5 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         12.60 μs
      </TD>
      <TD CLASS = "thinright">
         536.7 μs
      </TD>
      <TD CLASS = "thinright">
         18.78 ms
      </TD>
      <TD>
         556.0 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 24 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 129.50 times faster than Fgl
 * Hash-Graph was 1.01 times faster than Fgl

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
         36.30 ns
      </TD>
      <TD CLASS = "thinright">
         6.547 μs
      </TD>
      <TD CLASS = "thinright">
         1.074 ms
      </TD>
      <TD>
         294.6 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         67.90 ns
      </TD>
      <TD CLASS = "thinright">
         19.49 μs
      </TD>
      <TD CLASS = "thinright">
         10.40 ms
      </TD>
      <TD>
         3.414 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         74.52 ns
      </TD>
      <TD CLASS = "thinright">
         28.04 μs
      </TD>
      <TD CLASS = "thinright">
         8.298 ms
      </TD>
      <TD>
         3.668 s
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
         35.98 ns
      </TD>
      <TD CLASS = "thinright">
         1.875 μs
      </TD>
      <TD CLASS = "thinright">
         26.71 μs
      </TD>
      <TD>
         335.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         66.92 ns
      </TD>
      <TD CLASS = "thinright">
         6.304 μs
      </TD>
      <TD CLASS = "thinright">
         209.2 μs
      </TD>
      <TD>
         19.34 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         75.04 ns
      </TD>
      <TD CLASS = "thinright">
         8.172 μs
      </TD>
      <TD CLASS = "thinright">
         218.8 μs
      </TD>
      <TD>
         20.76 ms
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
         2.992 μs
      </TD>
      <TD CLASS = "thinright">
         59.31 μs
      </TD>
      <TD CLASS = "thinright">
         600.4 μs
      </TD>
      <TD>
         13.27 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         10.91 μs
      </TD>
      <TD CLASS = "thinright">
         525.3 μs
      </TD>
      <TD CLASS = "thinright">
         19.57 ms
      </TD>
      <TD>
         545.7 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         13.63 μs
      </TD>
      <TD CLASS = "thinright">
         542.7 μs
      </TD>
      <TD CLASS = "thinright">
         18.77 ms
      </TD>
      <TD>
         543.0 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 24 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 24.81 times faster than Hash-Graph
 * Fgl was 1.06 times faster than Hash-Graph

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
         22.65 μs
      </TD>
      <TD CLASS = "thinright">
         9.755 ms
      </TD>
      <TD>
         1.204 s
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         22.04 μs
      </TD>
      <TD CLASS = "thinright">
         10.61 ms
      </TD>
      <TD>
         3.490 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.42 μs
      </TD>
      <TD CLASS = "thinright">
         8.459 ms
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
         6.824 μs
      </TD>
      <TD CLASS = "thinright">
         93.62 μs
      </TD>
      <TD>
         3.084 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         7.195 μs
      </TD>
      <TD CLASS = "thinright">
         210.8 μs
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
         7.007 μs
      </TD>
      <TD CLASS = "thinright">
         216.7 μs
      </TD>
      <TD>
         18.61 ms
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
         10.73 μs
      </TD>
      <TD CLASS = "thinright">
         222.6 μs
      </TD>
      <TD CLASS = "thinright">
         5.626 ms
      </TD>
      <TD>
         61.31 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         11.72 μs
      </TD>
      <TD CLASS = "thinright">
         530.1 μs
      </TD>
      <TD CLASS = "thinright">
         20.56 ms
      </TD>
      <TD>
         545.6 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         12.40 μs
      </TD>
      <TD CLASS = "thinright">
         526.5 μs
      </TD>
      <TD CLASS = "thinright">
         17.44 ms
      </TD>
      <TD>
         543.2 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 20 times
 * Hash-Graph was the fastest 2 times

 There was 8 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 4.93 times faster than Hash-Graph
 * Fgl was 1.03 times faster than Hash-Graph

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
         112.0 ns
      </TD>
      <TD CLASS = "thinright">
         18.50 μs
      </TD>
      <TD CLASS = "thinright">
         11.18 ms
      </TD>
      <TD>
         3.502 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         117.7 ns
      </TD>
      <TD CLASS = "thinright">
         24.46 μs
      </TD>
      <TD CLASS = "thinright">
         8.564 ms
      </TD>
      <TD>
         3.622 s
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
         113.1 ns
      </TD>
      <TD CLASS = "thinright">
         6.089 μs
      </TD>
      <TD CLASS = "thinright">
         208.3 μs
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
         115.5 ns
      </TD>
      <TD CLASS = "thinright">
         7.144 μs
      </TD>
      <TD CLASS = "thinright">
         218.3 μs
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
         Fgl
      </TH>
      <TD CLASS = "thinright">
         10.58 μs
      </TD>
      <TD CLASS = "thinright">
         521.7 μs
      </TD>
      <TD CLASS = "thinright">
         19.25 ms
      </TD>
      <TD>
         544.5 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         12.40 μs
      </TD>
      <TD CLASS = "thinright">
         528.3 μs
      </TD>
      <TD CLASS = "thinright">
         17.47 ms
      </TD>
      <TD>
         544.7 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 11 times
 * Hash-Graph was the fastest 7 times

 There was 18 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Fgl was 1.02 times faster than Hash-Graph

