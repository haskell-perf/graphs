# Compare benchmarks

Doing:

----
* [isEmpty](#isempty)
* [vertexList](#vertexlist)
* [vertexCount](#vertexcount)
* [hasVertex](#hasvertex)
* [edgeCount](#edgecount)
* [edgeList](#edgelist)
* [hasEdge](#hasedge)
* [addEdge](#addedge)
* [addVertex](#addvertex)
* [removeVertex](#removevertex)
* [equality](#equality)
* [removeEdge](#removeedge)
* [transpose](#transpose)
* [dff](#dff)
* [topSort](#topsort)
* [reachable](#reachable)
* [mergeContext](#mergecontext)
* [creation](#creation)
----

Using [("Mesh",4),("Clique",4)] as graphs

## isEmpty

Descritpion: Test if the graph is empty

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
         22.13 ns
      </TD>
      <TD CLASS = "thinright">
         30.74 ns
      </TD>
      <TD CLASS = "thinright">
         31.15 ns
      </TD>
      <TD>
         30.04 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         20.30 ns
      </TD>
      <TD CLASS = "thinright">
         19.73 ns
      </TD>
      <TD CLASS = "thinright">
         19.35 ns
      </TD>
      <TD>
         19.36 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         19.53 ns
      </TD>
      <TD CLASS = "thinright">
         19.63 ns
      </TD>
      <TD CLASS = "thinright">
         19.43 ns
      </TD>
      <TD>
         20.57 ns
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         22.22 ns
      </TD>
      <TD CLASS = "thinright">
         30.15 ns
      </TD>
      <TD CLASS = "thinright">
         30.67 ns
      </TD>
      <TD>
         30.42 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         19.49 ns
      </TD>
      <TD CLASS = "thinright">
         19.63 ns
      </TD>
      <TD CLASS = "thinright">
         19.43 ns
      </TD>
      <TD>
         19.83 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         19.44 ns
      </TD>
      <TD CLASS = "thinright">
         19.44 ns
      </TD>
      <TD CLASS = "thinright">
         20.20 ns
      </TD>
      <TD>
         21.34 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 8 ex-aequo


ABSTRACT:

 * Fgl was 1.45 times faster than Alga
 * Hash-Graph was 1.43 times faster than Alga

## vertexList

Descritpion: Produce a list of the vertices in the graph

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
         48.79 ns
      </TD>
      <TD CLASS = "thinright">
         1.831 μs
      </TD>
      <TD CLASS = "thinright">
         47.66 μs
      </TD>
      <TD>
         1.085 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         33.03 ns
      </TD>
      <TD CLASS = "thinright">
         151.2 ns
      </TD>
      <TD CLASS = "thinright">
         1.107 μs
      </TD>
      <TD>
         10.32 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         51.69 ns
      </TD>
      <TD CLASS = "thinright">
         413.2 ns
      </TD>
      <TD CLASS = "thinright">
         4.363 μs
      </TD>
      <TD>
         41.37 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.62 ns
      </TD>
      <TD CLASS = "thinright">
         193.3 ns
      </TD>
      <TD CLASS = "thinright">
         2.383 μs
      </TD>
      <TD>
         24.25 μs
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         48.73 ns
      </TD>
      <TD CLASS = "thinright">
         5.005 μs
      </TD>
      <TD CLASS = "thinright">
         1.523 ms
      </TD>
      <TD>
         796.7 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         32.31 ns
      </TD>
      <TD CLASS = "thinright">
         149.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.064 μs
      </TD>
      <TD>
         10.63 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         50.32 ns
      </TD>
      <TD CLASS = "thinright">
         424.9 ns
      </TD>
      <TD CLASS = "thinright">
         4.190 μs
      </TD>
      <TD>
         42.15 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.51 ns
      </TD>
      <TD CLASS = "thinright">
         193.5 ns
      </TD>
      <TD CLASS = "thinright">
         2.220 μs
      </TD>
      <TD>
         24.51 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 6 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:

 * Containers was 9568.31 times faster than Alga
 * Hash-Graph was 4161.64 times faster than Alga
 * Fgl was 2415.23 times faster than Alga

## vertexCount

Descritpion: Count the vertices of the graph

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
         69.03 ns
      </TD>
      <TD CLASS = "thinright">
         3.187 μs
      </TD>
      <TD CLASS = "thinright">
         79.79 μs
      </TD>
      <TD>
         1.598 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         19.13 ns
      </TD>
      <TD CLASS = "thinright">
         19.12 ns
      </TD>
      <TD CLASS = "thinright">
         22.41 ns
      </TD>
      <TD>
         19.68 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         21.69 ns
      </TD>
      <TD CLASS = "thinright">
         103.5 ns
      </TD>
      <TD CLASS = "thinright">
         861.5 ns
      </TD>
      <TD>
         9.131 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         23.25 ns
      </TD>
      <TD CLASS = "thinright">
         86.25 ns
      </TD>
      <TD CLASS = "thinright">
         765.1 ns
      </TD>
      <TD>
         7.889 μs
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         68.69 ns
      </TD>
      <TD CLASS = "thinright">
         9.387 μs
      </TD>
      <TD CLASS = "thinright">
         2.609 ms
      </TD>
      <TD>
         893.1 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         18.91 ns
      </TD>
      <TD CLASS = "thinright">
         22.54 ns
      </TD>
      <TD CLASS = "thinright">
         19.46 ns
      </TD>
      <TD>
         19.34 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         21.32 ns
      </TD>
      <TD CLASS = "thinright">
         103.8 ns
      </TD>
      <TD CLASS = "thinright">
         844.4 ns
      </TD>
      <TD>
         9.626 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         23.10 ns
      </TD>
      <TD CLASS = "thinright">
         89.26 ns
      </TD>
      <TD CLASS = "thinright">
         746.8 ns
      </TD>
      <TD>
         8.061 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 8 times


ABSTRACT:

 * Containers was 5798354.24 times faster than Alga
 * Hash-Graph was 14342.61 times faster than Alga
 * Fgl was 12032.36 times faster than Alga

## hasVertex

Descritpion: Test if the given vertex is in the graph

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
         28.48 ns
      </TD>
      <TD CLASS = "thinright">
         171.2 ns
      </TD>
      <TD CLASS = "thinright">
         2.300 μs
      </TD>
      <TD>
         25.76 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         53.38 ns
      </TD>
      <TD CLASS = "thinright">
         224.9 ns
      </TD>
      <TD CLASS = "thinright">
         347.6 ns
      </TD>
      <TD>
         534.3 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.54 ns
      </TD>
      <TD CLASS = "thinright">
         34.20 ns
      </TD>
      <TD CLASS = "thinright">
         37.21 ns
      </TD>
      <TD>
         43.29 ns
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         28.53 ns
      </TD>
      <TD CLASS = "thinright">
         530.9 ns
      </TD>
      <TD CLASS = "thinright">
         43.52 μs
      </TD>
      <TD>
         5.165 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         54.53 ns
      </TD>
      <TD CLASS = "thinright">
         582.9 ns
      </TD>
      <TD CLASS = "thinright">
         9.935 μs
      </TD>
      <TD>
         295.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.98 ns
      </TD>
      <TD CLASS = "thinright">
         32.67 ns
      </TD>
      <TD CLASS = "thinright">
         37.12 ns
      </TD>
      <TD>
         41.51 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph was the fastest 12 times
 * Alga was the fastest 2 times
There was 8 ex-aequo


ABSTRACT:

 * Hash-Graph was 21790.31 times faster than Alga
 * Fgl was 15426.06 times faster than Alga

## edgeCount

Descritpion: Count the edges of the graph

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
         59.42 ns
      </TD>
      <TD CLASS = "thinright">
         9.712 μs
      </TD>
      <TD CLASS = "thinright">
         213.5 μs
      </TD>
      <TD>
         3.391 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         29.13 ns
      </TD>
      <TD CLASS = "thinright">
         127.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.174 μs
      </TD>
      <TD>
         12.43 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         58.23 ns
      </TD>
      <TD CLASS = "thinright">
         707.6 ns
      </TD>
      <TD CLASS = "thinright">
         9.067 μs
      </TD>
      <TD>
         96.48 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         38.63 ns
      </TD>
      <TD CLASS = "thinright">
         689.7 ns
      </TD>
      <TD CLASS = "thinright">
         8.419 μs
      </TD>
      <TD>
         109.0 μs
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         61.40 ns
      </TD>
      <TD CLASS = "thinright">
         36.09 μs
      </TD>
      <TD CLASS = "thinright">
         8.834 ms
      </TD>
      <TD>
         1.227 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         29.36 ns
      </TD>
      <TD CLASS = "thinright">
         263.1 ns
      </TD>
      <TD CLASS = "thinright">
         27.07 μs
      </TD>
      <TD>
         22.03 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         58.78 ns
      </TD>
      <TD CLASS = "thinright">
         1.897 μs
      </TD>
      <TD CLASS = "thinright">
         200.0 μs
      </TD>
      <TD>
         82.11 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         39.42 ns
      </TD>
      <TD CLASS = "thinright">
         951.1 ns
      </TD>
      <TD CLASS = "thinright">
         52.72 μs
      </TD>
      <TD>
         5.711 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 7 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:

 * Containers was 131.74 times faster than Alga
 * Hash-Graph was 61.74 times faster than Alga
 * Fgl was 19.08 times faster than Alga

## edgeList

Descritpion: Produce a list of the edges in the graph

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
         52.54 ns
      </TD>
      <TD CLASS = "thinright">
         10.18 μs
      </TD>
      <TD CLASS = "thinright">
         199.4 μs
      </TD>
      <TD>
         3.431 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         36.57 ns
      </TD>
      <TD CLASS = "thinright">
         380.8 ns
      </TD>
      <TD CLASS = "thinright">
         4.192 μs
      </TD>
      <TD>
         44.39 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         41.94 ns
      </TD>
      <TD CLASS = "thinright">
         928.7 ns
      </TD>
      <TD CLASS = "thinright">
         13.72 μs
      </TD>
      <TD>
         149.9 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         33.94 ns
      </TD>
      <TD CLASS = "thinright">
         668.0 ns
      </TD>
      <TD CLASS = "thinright">
         9.300 μs
      </TD>
      <TD>
         141.6 μs
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         52.31 ns
      </TD>
      <TD CLASS = "thinright">
         37.53 μs
      </TD>
      <TD CLASS = "thinright">
         8.100 ms
      </TD>
      <TD>
         1.128 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         36.95 ns
      </TD>
      <TD CLASS = "thinright">
         890.0 ns
      </TD>
      <TD CLASS = "thinright">
         81.66 μs
      </TD>
      <TD>
         24.54 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         42.60 ns
      </TD>
      <TD CLASS = "thinright">
         3.316 μs
      </TD>
      <TD CLASS = "thinright">
         352.9 μs
      </TD>
      <TD>
         76.86 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         33.41 ns
      </TD>
      <TD CLASS = "thinright">
         1.785 μs
      </TD>
      <TD CLASS = "thinright">
         300.7 μs
      </TD>
      <TD>
         135.8 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 6 times
 * Hash-Graph was the fastest 1 times
There was 1 ex-aequo


ABSTRACT:

 * Containers was 42.72 times faster than Alga
 * Hash-Graph was 15.04 times faster than Alga
 * Fgl was 12.48 times faster than Alga

## hasEdge

Descritpion: Test if the given edge is in the graph (with arguments both in the graph and not in the graph (where applicable))

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
         663.8 ns
      </TD>
      <TD CLASS = "thinright">
         8.226 μs
      </TD>
      <TD CLASS = "thinright">
         81.29 μs
      </TD>
      <TD>
         934.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         26.76 ns
      </TD>
      <TD CLASS = "thinright">
         47.57 ns
      </TD>
      <TD CLASS = "thinright">
         47.19 ns
      </TD>
      <TD>
         48.14 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         111.3 ns
      </TD>
      <TD CLASS = "thinright">
         453.7 ns
      </TD>
      <TD CLASS = "thinright">
         555.7 ns
      </TD>
      <TD>
         652.6 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         99.22 ns
      </TD>
      <TD CLASS = "thinright">
         146.4 ns
      </TD>
      <TD CLASS = "thinright">
         153.4 ns
      </TD>
      <TD>
         169.4 ns
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         668.8 ns
      </TD>
      <TD CLASS = "thinright">
         25.63 μs
      </TD>
      <TD CLASS = "thinright">
         2.755 ms
      </TD>
      <TD>
         277.1 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         26.72 ns
      </TD>
      <TD CLASS = "thinright">
         137.5 ns
      </TD>
      <TD CLASS = "thinright">
         1.249 μs
      </TD>
      <TD>
         14.41 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         110.2 ns
      </TD>
      <TD CLASS = "thinright">
         1.306 μs
      </TD>
      <TD CLASS = "thinright">
         19.26 μs
      </TD>
      <TD>
         464.9 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         101.7 ns
      </TD>
      <TD CLASS = "thinright">
         145.7 ns
      </TD>
      <TD CLASS = "thinright">
         159.8 ns
      </TD>
      <TD>
         171.7 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 16 times
 * Hash-Graph was the fastest 10 times


ABSTRACT:

 * Hash-Graph was 254804.89 times faster than Alga
 * Containers was 6676.83 times faster than Alga
 * Fgl was 367.10 times faster than Alga

## addEdge

Descritpion: Add an edge (not already in the graph)

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
         52.95 ns
      </TD>
      <TD CLASS = "thinright">
         346.8 ns
      </TD>
      <TD CLASS = "thinright">
         4.029 μs
      </TD>
      <TD>
         42.48 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         121.5 ns
      </TD>
      <TD CLASS = "thinright">
         1.001 μs
      </TD>
      <TD CLASS = "thinright">
         9.824 μs
      </TD>
      <TD>
         97.81 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         160.3 ns
      </TD>
      <TD CLASS = "thinright">
         1.607 μs
      </TD>
      <TD CLASS = "thinright">
         18.78 μs
      </TD>
      <TD>
         219.2 μs
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         54.67 ns
      </TD>
      <TD CLASS = "thinright">
         1.050 μs
      </TD>
      <TD CLASS = "thinright">
         109.6 μs
      </TD>
      <TD>
         16.29 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         122.6 ns
      </TD>
      <TD CLASS = "thinright">
         2.204 μs
      </TD>
      <TD CLASS = "thinright">
         204.1 μs
      </TD>
      <TD>
         30.19 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         161.9 ns
      </TD>
      <TD CLASS = "thinright">
         3.644 μs
      </TD>
      <TD CLASS = "thinright">
         372.0 μs
      </TD>
      <TD>
         40.83 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 26 times


ABSTRACT:

 * Alga was 3.67 times faster than Hash-Graph
 * Fgl was 1.67 times faster than Hash-Graph

## addVertex

Descritpion: Add a vertex (not already in the graph)

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
         40.70 ns
      </TD>
      <TD CLASS = "thinright">
         489.0 ns
      </TD>
      <TD CLASS = "thinright">
         3.954 μs
      </TD>
      <TD>
         42.07 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         84.04 ns
      </TD>
      <TD CLASS = "thinright">
         821.7 ns
      </TD>
      <TD CLASS = "thinright">
         9.208 μs
      </TD>
      <TD>
         94.65 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         106.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.385 μs
      </TD>
      <TD CLASS = "thinright">
         17.29 μs
      </TD>
      <TD>
         210.0 μs
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         43.48 ns
      </TD>
      <TD CLASS = "thinright">
         1.087 μs
      </TD>
      <TD CLASS = "thinright">
         115.7 μs
      </TD>
      <TD>
         13.63 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         78.35 ns
      </TD>
      <TD CLASS = "thinright">
         2.026 μs
      </TD>
      <TD CLASS = "thinright">
         201.5 μs
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
         105.0 ns
      </TD>
      <TD CLASS = "thinright">
         3.370 μs
      </TD>
      <TD CLASS = "thinright">
         344.0 μs
      </TD>
      <TD>
         38.90 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 8 times


ABSTRACT:

 * Alga was 3.09 times faster than Hash-Graph
 * Fgl was 1.58 times faster than Hash-Graph


## removeVertex

Descritpion: Remove a vertex of the graph

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
         34.17 ns
      </TD>
      <TD CLASS = "thinright">
         1.094 μs
      </TD>
      <TD CLASS = "thinright">
         15.56 μs
      </TD>
      <TD>
         175.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         42.79 ns
      </TD>
      <TD CLASS = "thinright">
         882.5 ns
      </TD>
      <TD CLASS = "thinright">
         9.411 μs
      </TD>
      <TD>
         95.99 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         51.99 ns
      </TD>
      <TD CLASS = "thinright">
         2.235 μs
      </TD>
      <TD CLASS = "thinright">
         20.22 μs
      </TD>
      <TD>
         225.6 μs
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         34.85 ns
      </TD>
      <TD CLASS = "thinright">
         3.803 μs
      </TD>
      <TD CLASS = "thinright">
         417.5 μs
      </TD>
      <TD>
         43.86 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         42.79 ns
      </TD>
      <TD CLASS = "thinright">
         2.282 μs
      </TD>
      <TD CLASS = "thinright">
         206.1 μs
      </TD>
      <TD>
         30.43 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         50.42 ns
      </TD>
      <TD CLASS = "thinright">
         6.289 μs
      </TD>
      <TD CLASS = "thinright">
         384.6 μs
      </TD>
      <TD>
         40.95 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 6 times
 * Alga was the fastest 2 times


ABSTRACT:

 * Fgl was 1.87 times faster than Hash-Graph
 * Alga was 1.30 times faster than Hash-Graph

## equality

Descritpion: Test if two graphs are equals

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
         211.6 ns
      </TD>
      <TD CLASS = "thinright">
         15.82 μs
      </TD>
      <TD CLASS = "thinright">
         306.2 μs
      </TD>
      <TD>
         5.458 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         30.62 ns
      </TD>
      <TD CLASS = "thinright">
         123.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.088 μs
      </TD>
      <TD>
         11.75 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         170.0 ns
      </TD>
      <TD CLASS = "thinright">
         4.891 μs
      </TD>
      <TD CLASS = "thinright">
         60.51 μs
      </TD>
      <TD>
         887.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         68.85 ns
      </TD>
      <TD CLASS = "thinright">
         1.708 μs
      </TD>
      <TD CLASS = "thinright">
         22.74 μs
      </TD>
      <TD>
         303.7 μs
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         213.6 ns
      </TD>
      <TD CLASS = "thinright">
         57.20 μs
      </TD>
      <TD CLASS = "thinright">
         12.94 ms
      </TD>
      <TD>
         1.887 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         30.53 ns
      </TD>
      <TD CLASS = "thinright">
         271.5 ns
      </TD>
      <TD CLASS = "thinright">
         25.02 μs
      </TD>
      <TD>
         11.00 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         168.5 ns
      </TD>
      <TD CLASS = "thinright">
         13.75 μs
      </TD>
      <TD CLASS = "thinright">
         2.332 ms
      </TD>
      <TD>
         357.5 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         67.91 ns
      </TD>
      <TD CLASS = "thinright">
         4.133 μs
      </TD>
      <TD CLASS = "thinright">
         455.7 μs
      </TD>
      <TD>
         57.78 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 14 times
There was 2 ex-aequo


ABSTRACT:

 * Containers was 2942971.04 times faster than Alga
 * Hash-Graph was 486232.82 times faster than Alga
 * Fgl was 1864.36 times faster than Alga

## removeEdge

Descritpion: Remove an edge of the graph

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
         5.378 μs
      </TD>
      <TD CLASS = "thinright">
         71.30 μs
      </TD>
      <TD>
         1.724 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.476 μs
      </TD>
      <TD CLASS = "thinright">
         10.30 μs
      </TD>
      <TD>
         96.87 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1.491 μs
      </TD>
      <TD CLASS = "thinright">
         19.82 μs
      </TD>
      <TD>
         228.0 μs
      </TD>
   </TR>
</TABLE>

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
         17.35 μs
      </TD>
      <TD CLASS = "thinright">
         6.559 ms
      </TD>
      <TD>
         832.2 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         4.470 μs
      </TD>
      <TD CLASS = "thinright">
         237.0 μs
      </TD>
      <TD>
         30.97 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         3.774 μs
      </TD>
      <TD CLASS = "thinright">
         334.6 μs
      </TD>
      <TD>
         37.97 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 12 times
 * Hash-Graph was the fastest 3 times
There was 3 ex-aequo


ABSTRACT:

 * Fgl was 14.47 times faster than Alga
 * Hash-Graph was 10.15 times faster than Alga

## transpose

Descritpion: Transpose (invert all the edges) the graph

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
         27.66 ns
      </TD>
      <TD CLASS = "thinright">
         767.2 ns
      </TD>
      <TD CLASS = "thinright">
         11.88 μs
      </TD>
      <TD>
         126.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         90.63 ns
      </TD>
      <TD CLASS = "thinright">
         898.6 ns
      </TD>
      <TD CLASS = "thinright">
         9.642 μs
      </TD>
      <TD>
         116.8 μs
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         26.90 ns
      </TD>
      <TD CLASS = "thinright">
         2.985 μs
      </TD>
      <TD CLASS = "thinright">
         299.7 μs
      </TD>
      <TD>
         32.31 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         89.74 ns
      </TD>
      <TD CLASS = "thinright">
         2.118 μs
      </TD>
      <TD CLASS = "thinright">
         266.9 μs
      </TD>
      <TD>
         67.66 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 4 times
 * Containers was the fastest 3 times
There was 1 ex-aequo


ABSTRACT:

 * Alga was 1.18 times faster than Containers

## dff

Descritpion: Produce a forest, obtainened from a DFS (Deep First Search) of each vertex

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
         116.1 ns
      </TD>
      <TD CLASS = "thinright">
         1.460 μs
      </TD>
      <TD CLASS = "thinright">
         14.38 μs
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
         200.0 ns
      </TD>
      <TD CLASS = "thinright">
         4.404 μs
      </TD>
      <TD CLASS = "thinright">
         67.84 μs
      </TD>
      <TD>
         891.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         64.56 ns
      </TD>
      <TD CLASS = "thinright">
         2.648 μs
      </TD>
      <TD CLASS = "thinright">
         49.82 μs
      </TD>
      <TD>
         1.441 ms
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
         114.9 ns
      </TD>
      <TD CLASS = "thinright">
         2.462 μs
      </TD>
      <TD CLASS = "thinright">
         172.3 μs
      </TD>
      <TD>
         29.86 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         202.0 ns
      </TD>
      <TD CLASS = "thinright">
         5.826 μs
      </TD>
      <TD CLASS = "thinright">
         558.5 μs
      </TD>
      <TD>
         120.9 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         65.13 ns
      </TD>
      <TD CLASS = "thinright">
         5.595 μs
      </TD>
      <TD CLASS = "thinright">
         556.8 μs
      </TD>
      <TD>
         94.09 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 6 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:

 * Containers was 2.80 times faster than Fgl
 * Hash-Graph was 1.96 times faster than Fgl

## topSort

Descritpion: Topological sorting of the vertices

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
         141.3 ns
      </TD>
      <TD CLASS = "thinright">
         1.553 μs
      </TD>
      <TD CLASS = "thinright">
         15.78 μs
      </TD>
      <TD>
         180.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         333.2 ns
      </TD>
      <TD CLASS = "thinright">
         6.789 μs
      </TD>
      <TD CLASS = "thinright">
         111.6 μs
      </TD>
      <TD>
         2.502 ms
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
         138.2 ns
      </TD>
      <TD CLASS = "thinright">
         2.545 μs
      </TD>
      <TD CLASS = "thinright">
         173.9 μs
      </TD>
      <TD>
         29.75 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         337.1 ns
      </TD>
      <TD CLASS = "thinright">
         9.377 μs
      </TD>
      <TD CLASS = "thinright">
         804.5 μs
      </TD>
      <TD>
         143.2 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 8 times


ABSTRACT:

 * Containers was 4.06 times faster than Fgl

## reachable

Descritpion: Produce a list of reachable vertices from a given one

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
         124.5 ns
      </TD>
      <TD CLASS = "thinright">
         1.092 μs
      </TD>
      <TD CLASS = "thinright">
         10.90 μs
      </TD>
      <TD>
         121.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         250.1 ns
      </TD>
      <TD CLASS = "thinright">
         5.769 μs
      </TD>
      <TD CLASS = "thinright">
         82.16 μs
      </TD>
      <TD>
         1.273 ms
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
         121.5 ns
      </TD>
      <TD CLASS = "thinright">
         2.148 μs
      </TD>
      <TD CLASS = "thinright">
         169.7 μs
      </TD>
      <TD>
         31.53 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         248.8 ns
      </TD>
      <TD CLASS = "thinright">
         7.778 μs
      </TD>
      <TD CLASS = "thinright">
         608.7 μs
      </TD>
      <TD>
         123.1 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 8 times


ABSTRACT:

 * Containers was 3.61 times faster than Fgl

## mergeContext

Descritpion: Merge a FGL context in the graph

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
         93.71 ns
      </TD>
      <TD CLASS = "thinright">
         933.9 ns
      </TD>
      <TD CLASS = "thinright">
         9.478 μs
      </TD>
      <TD>
         95.78 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         77.61 ns
      </TD>
      <TD CLASS = "thinright">
         1.289 μs
      </TD>
      <TD CLASS = "thinright">
         18.82 μs
      </TD>
      <TD>
         234.9 μs
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
         Fgl
      </TH>
      <TD CLASS = "thinright">
         99.20 ns
      </TD>
      <TD CLASS = "thinright">
         1.977 μs
      </TD>
      <TD CLASS = "thinright">
         198.8 μs
      </TD>
      <TD>
         27.81 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         79.91 ns
      </TD>
      <TD CLASS = "thinright">
         3.466 μs
      </TD>
      <TD CLASS = "thinright">
         324.1 μs
      </TD>
      <TD>
         39.93 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 6 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:

 * Fgl was 1.34 times faster than Hash-Graph

## creation

Descritpion: Create a graph from a list of edges

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
         25.85 ns
      </TD>
      <TD CLASS = "thinright">
         1.986 μs
      </TD>
      <TD CLASS = "thinright">
         27.85 μs
      </TD>
      <TD>
         296.6 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         56.06 ns
      </TD>
      <TD CLASS = "thinright">
         534.9 ns
      </TD>
      <TD CLASS = "thinright">
         6.525 μs
      </TD>
      <TD>
         72.73 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         34.25 ns
      </TD>
      <TD CLASS = "thinright">
         6.098 μs
      </TD>
      <TD CLASS = "thinright">
         200.7 μs
      </TD>
      <TD>
         17.90 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.88 ns
      </TD>
      <TD CLASS = "thinright">
         6.519 μs
      </TD>
      <TD CLASS = "thinright">
         205.7 μs
      </TD>
      <TD>
         18.01 ms
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         25.81 ns
      </TD>
      <TD CLASS = "thinright">
         6.710 μs
      </TD>
      <TD CLASS = "thinright">
         784.9 μs
      </TD>
      <TD>
         74.05 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         56.04 ns
      </TD>
      <TD CLASS = "thinright">
         1.290 μs
      </TD>
      <TD CLASS = "thinright">
         116.3 μs
      </TD>
      <TD>
         51.62 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         34.21 ns
      </TD>
      <TD CLASS = "thinright">
         18.81 μs
      </TD>
      <TD CLASS = "thinright">
         7.620 ms
      </TD>
      <TD>
         3.179 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         26.37 ns
      </TD>
      <TD CLASS = "thinright">
         22.68 μs
      </TD>
      <TD CLASS = "thinright">
         6.709 ms
      </TD>
      <TD>
         3.494 s
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 6 times
There was 2 ex-aequo


ABSTRACT:

 * Containers was 10.66 times faster than Fgl
 * Alga was 3.23 times faster than Fgl
 * Hash-Graph was 1.16 times faster than Fgl

