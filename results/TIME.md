# Compare benchmarks

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
* [creation](#creation)
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
         47.38 ns
      </TD>
      <TD CLASS = "thinright">
         38.04 μs
      </TD>
      <TD CLASS = "thinright">
         7.637 ms
      </TD>
      <TD>
         1.161 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         38.96 ns
      </TD>
      <TD CLASS = "thinright">
         887.6 ns
      </TD>
      <TD CLASS = "thinright">
         79.68 μs
      </TD>
      <TD>
         25.51 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         33.53 ns
      </TD>
      <TD CLASS = "thinright">
         2.884 μs
      </TD>
      <TD CLASS = "thinright">
         329.9 μs
      </TD>
      <TD>
         119.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         41.38 ns
      </TD>
      <TD CLASS = "thinright">
         1.826 μs
      </TD>
      <TD CLASS = "thinright">
         303.4 μs
      </TD>
      <TD>
         213.6 ms
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
         47.46 ns
      </TD>
      <TD CLASS = "thinright">
         9.892 μs
      </TD>
      <TD CLASS = "thinright">
         199.8 μs
      </TD>
      <TD>
         3.386 ms
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
         396.9 ns
      </TD>
      <TD CLASS = "thinright">
         4.266 μs
      </TD>
      <TD>
         45.18 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         33.41 ns
      </TD>
      <TD CLASS = "thinright">
         929.2 ns
      </TD>
      <TD CLASS = "thinright">
         12.34 μs
      </TD>
      <TD>
         134.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         37.39 ns
      </TD>
      <TD CLASS = "thinright">
         700.7 ns
      </TD>
      <TD CLASS = "thinright">
         9.647 μs
      </TD>
      <TD>
         148.2 μs
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
         84.19 ms
      </TD>
      <TD CLASS = "thinright">
         17.12 μs
      </TD>
      <TD CLASS = "thinright">
         538.9 μs
      </TD>
      <TD>
         6.354 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         462.9 μs
      </TD>
      <TD CLASS = "thinright">
         578.5 ns
      </TD>
      <TD CLASS = "thinright">
         7.925 μs
      </TD>
      <TD>
         56.17 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         4.677 ms
      </TD>
      <TD CLASS = "thinright">
         1.448 μs
      </TD>
      <TD CLASS = "thinright">
         26.27 μs
      </TD>
      <TD>
         245.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         8.579 ms
      </TD>
      <TD CLASS = "thinright">
         1.065 μs
      </TD>
      <TD CLASS = "thinright">
         20.03 μs
      </TD>
      <TD>
         205.3 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Fgl was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 67.94 times faster than Alga
 * Fgl was 17.66 times faster than Alga
 * Hash-Graph was 15.80 times faster than Alga

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
         47.15 ns
      </TD>
      <TD CLASS = "thinright">
         18.42 μs
      </TD>
      <TD CLASS = "thinright">
         3.874 ms
      </TD>
      <TD>
         631.6 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         33.89 ns
      </TD>
      <TD CLASS = "thinright">
         149.5 ns
      </TD>
      <TD CLASS = "thinright">
         1.062 μs
      </TD>
      <TD>
         10.60 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         56.46 ns
      </TD>
      <TD CLASS = "thinright">
         418.1 ns
      </TD>
      <TD CLASS = "thinright">
         4.226 μs
      </TD>
      <TD>
         43.22 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.66 ns
      </TD>
      <TD CLASS = "thinright">
         195.3 ns
      </TD>
      <TD CLASS = "thinright">
         2.220 μs
      </TD>
      <TD>
         27.13 μs
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
         46.30 ns
      </TD>
      <TD CLASS = "thinright">
         5.231 μs
      </TD>
      <TD CLASS = "thinright">
         133.0 μs
      </TD>
      <TD>
         2.200 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         33.00 ns
      </TD>
      <TD CLASS = "thinright">
         151.1 ns
      </TD>
      <TD CLASS = "thinright">
         1.090 μs
      </TD>
      <TD>
         10.38 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         52.39 ns
      </TD>
      <TD CLASS = "thinright">
         421.9 ns
      </TD>
      <TD CLASS = "thinright">
         4.262 μs
      </TD>
      <TD>
         43.25 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.31 ns
      </TD>
      <TD CLASS = "thinright">
         195.9 ns
      </TD>
      <TD CLASS = "thinright">
         2.189 μs
      </TD>
      <TD>
         25.67 μs
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
         32.13 ms
      </TD>
      <TD CLASS = "thinright">
         9.509 μs
      </TD>
      <TD CLASS = "thinright">
         277.1 μs
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
         16.87 μs
      </TD>
      <TD CLASS = "thinright">
         188.0 ns
      </TD>
      <TD CLASS = "thinright">
         936.4 ns
      </TD>
      <TD>
         3.699 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         70.55 μs
      </TD>
      <TD CLASS = "thinright">
         595.3 ns
      </TD>
      <TD CLASS = "thinright">
         3.753 μs
      </TD>
      <TD>
         14.91 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         36.07 μs
      </TD>
      <TD CLASS = "thinright">
         274.1 ns
      </TD>
      <TD CLASS = "thinright">
         1.980 μs
      </TD>
      <TD>
         9.541 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 501.50 times faster than Alga
 * Hash-Graph was 197.71 times faster than Alga
 * Fgl was 122.58 times faster than Alga

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
         197.7 ns
      </TD>
      <TD CLASS = "thinright">
         59.32 μs
      </TD>
      <TD CLASS = "thinright">
         11.17 ms
      </TD>
      <TD>
         1.863 s
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
         265.1 ns
      </TD>
      <TD CLASS = "thinright">
         23.65 μs
      </TD>
      <TD>
         9.990 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         164.5 ns
      </TD>
      <TD CLASS = "thinright">
         14.50 μs
      </TD>
      <TD CLASS = "thinright">
         2.712 ms
      </TD>
      <TD>
         499.8 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         71.46 ns
      </TD>
      <TD CLASS = "thinright">
         4.325 μs
      </TD>
      <TD CLASS = "thinright">
         449.4 μs
      </TD>
      <TD>
         56.78 ms
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
         195.8 ns
      </TD>
      <TD CLASS = "thinright">
         15.87 μs
      </TD>
      <TD CLASS = "thinright">
         310.9 μs
      </TD>
      <TD>
         4.931 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         30.55 ns
      </TD>
      <TD CLASS = "thinright">
         121.6 ns
      </TD>
      <TD CLASS = "thinright">
         1.151 μs
      </TD>
      <TD>
         11.87 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         160.3 ns
      </TD>
      <TD CLASS = "thinright">
         5.058 μs
      </TD>
      <TD CLASS = "thinright">
         63.68 μs
      </TD>
      <TD>
         914.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         71.31 ns
      </TD>
      <TD CLASS = "thinright">
         1.723 μs
      </TD>
      <TD CLASS = "thinright">
         21.87 μs
      </TD>
      <TD>
         301.6 μs
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
         108.7 ms
      </TD>
      <TD CLASS = "thinright">
         27.46 μs
      </TD>
      <TD CLASS = "thinright">
         810.7 μs
      </TD>
      <TD>
         9.172 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         155.6 μs
      </TD>
      <TD CLASS = "thinright">
         177.0 ns
      </TD>
      <TD CLASS = "thinright">
         2.536 μs
      </TD>
      <TD>
         18.65 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         25.39 ms
      </TD>
      <TD CLASS = "thinright">
         7.706 μs
      </TD>
      <TD CLASS = "thinright">
         136.3 μs
      </TD>
      <TD>
         2.056 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         3.214 ms
      </TD>
      <TD CLASS = "thinright">
         2.601 μs
      </TD>
      <TD CLASS = "thinright">
         41.43 μs
      </TD>
      <TD>
         340.2 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 22 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 424.57 times faster than Alga
 * Hash-Graph was 57.55 times faster than Alga
 * Fgl was 3.22 times faster than Alga

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
         28.43 ns
      </TD>
      <TD CLASS = "thinright">
         3.148 μs
      </TD>
      <TD CLASS = "thinright">
         295.3 μs
      </TD>
      <TD>
         30.08 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         90.30 ns
      </TD>
      <TD CLASS = "thinright">
         2.193 μs
      </TD>
      <TD CLASS = "thinright">
         264.2 μs
      </TD>
      <TD>
         87.58 ms
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
         28.33 ns
      </TD>
      <TD CLASS = "thinright">
         905.4 ns
      </TD>
      <TD CLASS = "thinright">
         12.19 μs
      </TD>
      <TD>
         123.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         89.34 ns
      </TD>
      <TD CLASS = "thinright">
         940.8 ns
      </TD>
      <TD CLASS = "thinright">
         9.721 μs
      </TD>
      <TD>
         119.3 μs
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
         1.597 ms
      </TD>
      <TD CLASS = "thinright">
         1.431 μs
      </TD>
      <TD CLASS = "thinright">
         27.04 μs
      </TD>
      <TD>
         219.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         3.575 ms
      </TD>
      <TD CLASS = "thinright">
         1.384 μs
      </TD>
      <TD CLASS = "thinright">
         22.05 μs
      </TD>
      <TD>
         203.5 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 4 times
 * Containers was the fastest 4 times

 There was 4 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 1.60 times faster than Containers

## dff

Faulty bench

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
         142.4 ns
      </TD>
      <TD CLASS = "thinright">
         2.558 μs
      </TD>
      <TD CLASS = "thinright">
         170.3 μs
      </TD>
      <TD>
         31.20 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         348.9 ns
      </TD>
      <TD CLASS = "thinright">
         9.475 μs
      </TD>
      <TD CLASS = "thinright">
         821.5 μs
      </TD>
      <TD>
         143.2 ms
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
         149.6 ns
      </TD>
      <TD CLASS = "thinright">
         1.581 μs
      </TD>
      <TD CLASS = "thinright">
         15.89 μs
      </TD>
      <TD>
         179.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         352.1 ns
      </TD>
      <TD CLASS = "thinright">
         6.911 μs
      </TD>
      <TD CLASS = "thinright">
         112.1 μs
      </TD>
      <TD>
         3.984 ms
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
         3.415 ms
      </TD>
      <TD CLASS = "thinright">
         2.418 μs
      </TD>
      <TD CLASS = "thinright">
         24.74 μs
      </TD>
      <TD>
         171.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         39.87 ms
      </TD>
      <TD CLASS = "thinright">
         11.49 μs
      </TD>
      <TD CLASS = "thinright">
         189.1 μs
      </TD>
      <TD>
         3.042 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 12 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 9.40 times faster than Fgl

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
         1.116 μs
      </TD>
      <TD CLASS = "thinright">
         84.79 μs
      </TD>
      <TD>
         13.14 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         260.4 ns
      </TD>
      <TD CLASS = "thinright">
         4.709 μs
      </TD>
      <TD CLASS = "thinright">
         327.1 μs
      </TD>
      <TD>
         62.25 ms
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
         128.0 ns
      </TD>
      <TD CLASS = "thinright">
         629.0 ns
      </TD>
      <TD CLASS = "thinright">
         5.624 μs
      </TD>
      <TD>
         61.14 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         260.8 ns
      </TD>
      <TD CLASS = "thinright">
         3.328 μs
      </TD>
      <TD CLASS = "thinright">
         42.01 μs
      </TD>
      <TD>
         729.8 μs
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
         1.219 ms
      </TD>
      <TD CLASS = "thinright">
         900.0 ns
      </TD>
      <TD CLASS = "thinright">
         18.30 μs
      </TD>
      <TD>
         72.27 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         12.71 ms
      </TD>
      <TD CLASS = "thinright">
         5.094 μs
      </TD>
      <TD CLASS = "thinright">
         147.5 μs
      </TD>
      <TD>
         1.102 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 21 times
 * Fgl was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 2.97 times faster than Fgl

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
         30.90 ns
      </TD>
      <TD CLASS = "thinright">
         17.90 μs
      </TD>
      <TD CLASS = "thinright">
         3.956 ms
      </TD>
      <TD>
         618.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         20.21 ns
      </TD>
      <TD CLASS = "thinright">
         20.82 ns
      </TD>
      <TD CLASS = "thinright">
         20.17 ns
      </TD>
      <TD>
         21.02 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         23.71 ns
      </TD>
      <TD CLASS = "thinright">
         98.95 ns
      </TD>
      <TD CLASS = "thinright">
         810.7 ns
      </TD>
      <TD>
         9.267 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.47 ns
      </TD>
      <TD CLASS = "thinright">
         88.35 ns
      </TD>
      <TD CLASS = "thinright">
         793.2 ns
      </TD>
      <TD>
         9.110 μs
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
         30.66 ns
      </TD>
      <TD CLASS = "thinright">
         5.178 μs
      </TD>
      <TD CLASS = "thinright">
         131.3 μs
      </TD>
      <TD>
         2.186 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         20.19 ns
      </TD>
      <TD CLASS = "thinright">
         20.17 ns
      </TD>
      <TD CLASS = "thinright">
         20.27 ns
      </TD>
      <TD>
         20.17 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         23.82 ns
      </TD>
      <TD CLASS = "thinright">
         98.40 ns
      </TD>
      <TD CLASS = "thinright">
         859.8 ns
      </TD>
      <TD>
         8.932 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.17 ns
      </TD>
      <TD CLASS = "thinright">
         86.27 ns
      </TD>
      <TD CLASS = "thinright">
         794.1 ns
      </TD>
      <TD>
         8.667 μs
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
         32.00 ms
      </TD>
      <TD CLASS = "thinright">
         9.150 μs
      </TD>
      <TD CLASS = "thinright">
         271.5 μs
      </TD>
      <TD>
         2.971 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         20.16 ns
      </TD>
      <TD CLASS = "thinright">
         20.18 ns
      </TD>
      <TD CLASS = "thinright">
         20.17 ns
      </TD>
      <TD>
         20.17 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         14.95 μs
      </TD>
      <TD CLASS = "thinright">
         130.2 ns
      </TD>
      <TD CLASS = "thinright">
         702.0 ns
      </TD>
      <TD>
         3.139 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         13.72 μs
      </TD>
      <TD CLASS = "thinright">
         118.2 ns
      </TD>
      <TD CLASS = "thinright">
         712.5 ns
      </TD>
      <TD>
         3.200 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 12 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 186906.77 times faster than Alga
 * Fgl was 682.35 times faster than Alga
 * Hash-Graph was 681.37 times faster than Alga

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
         53.83 ns
      </TD>
      <TD CLASS = "thinright">
         38.05 μs
      </TD>
      <TD CLASS = "thinright">
         8.564 ms
      </TD>
      <TD>
         1.268 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         29.98 ns
      </TD>
      <TD CLASS = "thinright">
         251.2 ns
      </TD>
      <TD CLASS = "thinright">
         25.46 μs
      </TD>
      <TD>
         22.18 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         34.47 ns
      </TD>
      <TD CLASS = "thinright">
         1.905 μs
      </TD>
      <TD CLASS = "thinright">
         209.8 μs
      </TD>
      <TD>
         108.5 ms
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
         946.3 ns
      </TD>
      <TD CLASS = "thinright">
         57.37 μs
      </TD>
      <TD>
         6.249 ms
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
         53.37 ns
      </TD>
      <TD CLASS = "thinright">
         9.887 μs
      </TD>
      <TD CLASS = "thinright">
         199.2 μs
      </TD>
      <TD>
         3.215 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         29.23 ns
      </TD>
      <TD CLASS = "thinright">
         122.6 ns
      </TD>
      <TD CLASS = "thinright">
         1.216 μs
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
         34.58 ns
      </TD>
      <TD CLASS = "thinright">
         705.2 ns
      </TD>
      <TD CLASS = "thinright">
         9.158 μs
      </TD>
      <TD>
         99.16 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         39.14 ns
      </TD>
      <TD CLASS = "thinright">
         689.3 ns
      </TD>
      <TD CLASS = "thinright">
         8.580 μs
      </TD>
      <TD>
         110.0 μs
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
         71.76 ms
      </TD>
      <TD CLASS = "thinright">
         16.98 μs
      </TD>
      <TD CLASS = "thinright">
         530.7 μs
      </TD>
      <TD>
         5.977 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         133.7 μs
      </TD>
      <TD CLASS = "thinright">
         193.5 ns
      </TD>
      <TD CLASS = "thinright">
         2.163 μs
      </TD>
      <TD>
         17.97 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         3.799 ms
      </TD>
      <TD CLASS = "thinright">
         1.019 μs
      </TD>
      <TD CLASS = "thinright">
         19.01 μs
      </TD>
      <TD>
         145.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         525.4 μs
      </TD>
      <TD CLASS = "thinright">
         1.027 μs
      </TD>
      <TD CLASS = "thinright">
         11.27 μs
      </TD>
      <TD>
         74.85 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 11 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 160.19 times faster than Alga
 * Containers was 123.13 times faster than Alga
 * Fgl was 18.62 times faster than Alga

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
         648.6 ns
      </TD>
      <TD CLASS = "thinright">
         8.906 μs
      </TD>
      <TD CLASS = "thinright">
         658.3 μs
      </TD>
      <TD>
         101.3 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         29.54 ns
      </TD>
      <TD CLASS = "thinright">
         118.3 ns
      </TD>
      <TD CLASS = "thinright">
         1.024 μs
      </TD>
      <TD>
         10.46 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         115.2 ns
      </TD>
      <TD CLASS = "thinright">
         1.384 μs
      </TD>
      <TD CLASS = "thinright">
         18.69 μs
      </TD>
      <TD>
         462.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         112.4 ns
      </TD>
      <TD CLASS = "thinright">
         148.5 ns
      </TD>
      <TD CLASS = "thinright">
         164.0 ns
      </TD>
      <TD>
         174.4 ns
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
         669.0 ns
      </TD>
      <TD CLASS = "thinright">
         3.477 μs
      </TD>
      <TD CLASS = "thinright">
         25.02 μs
      </TD>
      <TD>
         259.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         29.67 ns
      </TD>
      <TD CLASS = "thinright">
         51.88 ns
      </TD>
      <TD CLASS = "thinright">
         52.19 ns
      </TD>
      <TD>
         50.27 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         114.9 ns
      </TD>
      <TD CLASS = "thinright">
         503.9 ns
      </TD>
      <TD CLASS = "thinright">
         599.2 ns
      </TD>
      <TD>
         753.6 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         111.0 ns
      </TD>
      <TD CLASS = "thinright">
         146.9 ns
      </TD>
      <TD CLASS = "thinright">
         152.7 ns
      </TD>
      <TD>
         168.5 ns
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
         3.352 ms
      </TD>
      <TD CLASS = "thinright">
         4.533 μs
      </TD>
      <TD CLASS = "thinright">
         56.42 μs
      </TD>
      <TD>
         435.7 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         164.3 ns
      </TD>
      <TD CLASS = "thinright">
         58.57 ns
      </TD>
      <TD CLASS = "thinright">
         50.41 ns
      </TD>
      <TD>
         90.92 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         8.803 μs
      </TD>
      <TD CLASS = "thinright">
         513.5 ns
      </TD>
      <TD CLASS = "thinright">
         1.275 μs
      </TD>
      <TD>
         6.316 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         167.3 ns
      </TD>
      <TD CLASS = "thinright">
         145.2 ns
      </TD>
      <TD CLASS = "thinright">
         155.4 ns
      </TD>
      <TD>
         167.6 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 46 times
 * Hash-Graph was the fastest 14 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 130653.38 times faster than Alga
 * Containers was 5912.67 times faster than Alga
 * Fgl was 213.83 times faster than Alga

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
         21.51 ns
      </TD>
      <TD CLASS = "thinright">
         37.80 ns
      </TD>
      <TD CLASS = "thinright">
         32.15 ns
      </TD>
      <TD>
         32.92 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         20.87 ns
      </TD>
      <TD CLASS = "thinright">
         21.89 ns
      </TD>
      <TD CLASS = "thinright">
         21.37 ns
      </TD>
      <TD>
         21.41 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         20.40 ns
      </TD>
      <TD CLASS = "thinright">
         20.60 ns
      </TD>
      <TD CLASS = "thinright">
         20.90 ns
      </TD>
      <TD>
         20.44 ns
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
         21.08 ns
      </TD>
      <TD CLASS = "thinright">
         32.40 ns
      </TD>
      <TD CLASS = "thinright">
         32.38 ns
      </TD>
      <TD>
         32.23 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         20.08 ns
      </TD>
      <TD CLASS = "thinright">
         21.37 ns
      </TD>
      <TD CLASS = "thinright">
         20.64 ns
      </TD>
      <TD>
         20.69 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         20.89 ns
      </TD>
      <TD CLASS = "thinright">
         20.49 ns
      </TD>
      <TD CLASS = "thinright">
         20.69 ns
      </TD>
      <TD>
         20.75 ns
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
         32.61 ns
      </TD>
      <TD CLASS = "thinright">
         32.33 ns
      </TD>
      <TD CLASS = "thinright">
         32.58 ns
      </TD>
      <TD>
         32.22 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         20.63 ns
      </TD>
      <TD CLASS = "thinright">
         20.73 ns
      </TD>
      <TD CLASS = "thinright">
         21.11 ns
      </TD>
      <TD>
         20.66 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         21.73 ns
      </TD>
      <TD CLASS = "thinright">
         20.85 ns
      </TD>
      <TD CLASS = "thinright">
         21.40 ns
      </TD>
      <TD>
         20.33 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:


 There was 12 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 1.58 times faster than Alga
 * Fgl was 1.55 times faster than Alga

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
         33.00 ns
      </TD>
      <TD CLASS = "thinright">
         544.7 ns
      </TD>
      <TD CLASS = "thinright">
         44.83 μs
      </TD>
      <TD>
         4.617 ms
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
         719.6 ns
      </TD>
      <TD CLASS = "thinright">
         11.49 μs
      </TD>
      <TD>
         327.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.80 ns
      </TD>
      <TD CLASS = "thinright">
         34.19 ns
      </TD>
      <TD CLASS = "thinright">
         38.06 ns
      </TD>
      <TD>
         42.53 ns
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
         32.93 ns
      </TD>
      <TD CLASS = "thinright">
         249.6 ns
      </TD>
      <TD CLASS = "thinright">
         3.035 μs
      </TD>
      <TD>
         34.83 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         53.70 ns
      </TD>
      <TD CLASS = "thinright">
         248.9 ns
      </TD>
      <TD CLASS = "thinright">
         357.8 ns
      </TD>
      <TD>
         568.9 ns
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
         34.39 ns
      </TD>
      <TD CLASS = "thinright">
         37.70 ns
      </TD>
      <TD>
         42.58 ns
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
         286.4 μs
      </TD>
      <TD CLASS = "thinright">
         381.5 ns
      </TD>
      <TD CLASS = "thinright">
         4.868 μs
      </TD>
      <TD>
         57.92 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         3.535 μs
      </TD>
      <TD CLASS = "thinright">
         253.0 ns
      </TD>
      <TD CLASS = "thinright">
         987.4 ns
      </TD>
      <TD>
         1.745 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         41.37 ns
      </TD>
      <TD CLASS = "thinright">
         36.08 ns
      </TD>
      <TD CLASS = "thinright">
         37.93 ns
      </TD>
      <TD>
         36.32 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph was the fastest 39 times

 There was 5 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 47314.43 times faster than Alga
 * Fgl was 33724.86 times faster than Alga

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
         49.39 ns
      </TD>
      <TD CLASS = "thinright">
         1.085 μs
      </TD>
      <TD CLASS = "thinright">
         113.3 μs
      </TD>
      <TD>
         13.70 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         120.7 ns
      </TD>
      <TD CLASS = "thinright">
         2.083 μs
      </TD>
      <TD CLASS = "thinright">
         192.3 μs
      </TD>
      <TD>
         28.05 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         203.6 ns
      </TD>
      <TD CLASS = "thinright">
         3.952 μs
      </TD>
      <TD CLASS = "thinright">
         359.1 μs
      </TD>
      <TD>
         39.70 ms
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
         49.40 ns
      </TD>
      <TD CLASS = "thinright">
         351.9 ns
      </TD>
      <TD CLASS = "thinright">
         4.220 μs
      </TD>
      <TD>
         44.49 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         116.4 ns
      </TD>
      <TD CLASS = "thinright">
         990.8 ns
      </TD>
      <TD CLASS = "thinright">
         9.360 μs
      </TD>
      <TD>
         93.50 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         192.2 ns
      </TD>
      <TD CLASS = "thinright">
         1.787 μs
      </TD>
      <TD CLASS = "thinright">
         19.38 μs
      </TD>
      <TD>
         228.0 μs
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
         638.1 μs
      </TD>
      <TD CLASS = "thinright">
         537.0 ns
      </TD>
      <TD CLASS = "thinright">
         9.398 μs
      </TD>
      <TD>
         74.05 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.284 ms
      </TD>
      <TD CLASS = "thinright">
         1.412 μs
      </TD>
      <TD CLASS = "thinright">
         19.43 μs
      </TD>
      <TD>
         142.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.373 ms
      </TD>
      <TD CLASS = "thinright">
         2.516 μs
      </TD>
      <TD CLASS = "thinright">
         36.03 μs
      </TD>
      <TD>
         274.2 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 42 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 3.70 times faster than Hash-Graph
 * Fgl was 1.83 times faster than Hash-Graph

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
         42.39 ns
      </TD>
      <TD CLASS = "thinright">
         1.072 μs
      </TD>
      <TD CLASS = "thinright">
         113.0 μs
      </TD>
      <TD>
         17.26 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         76.14 ns
      </TD>
      <TD CLASS = "thinright">
         1.871 μs
      </TD>
      <TD CLASS = "thinright">
         183.5 μs
      </TD>
      <TD>
         27.95 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         118.0 ns
      </TD>
      <TD CLASS = "thinright">
         3.661 μs
      </TD>
      <TD CLASS = "thinright">
         350.4 μs
      </TD>
      <TD>
         40.19 ms
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
         42.51 ns
      </TD>
      <TD CLASS = "thinright">
         345.4 ns
      </TD>
      <TD CLASS = "thinright">
         4.225 μs
      </TD>
      <TD>
         45.02 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         72.68 ns
      </TD>
      <TD CLASS = "thinright">
         781.9 ns
      </TD>
      <TD CLASS = "thinright">
         8.889 μs
      </TD>
      <TD>
         92.56 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         118.3 ns
      </TD>
      <TD CLASS = "thinright">
         1.521 μs
      </TD>
      <TD CLASS = "thinright">
         18.75 μs
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
         674.7 μs
      </TD>
      <TD CLASS = "thinright">
         522.6 ns
      </TD>
      <TD CLASS = "thinright">
         9.276 μs
      </TD>
      <TD>
         73.86 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.272 ms
      </TD>
      <TD CLASS = "thinright">
         1.216 μs
      </TD>
      <TD CLASS = "thinright">
         19.34 μs
      </TD>
      <TD>
         142.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.337 ms
      </TD>
      <TD CLASS = "thinright">
         2.336 μs
      </TD>
      <TD CLASS = "thinright">
         35.28 μs
      </TD>
      <TD>
         273.1 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 24 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 3.34 times faster than Hash-Graph
 * Fgl was 1.81 times faster than Hash-Graph

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
         34.90 ns
      </TD>
      <TD CLASS = "thinright">
         3.412 μs
      </TD>
      <TD CLASS = "thinright">
         447.9 μs
      </TD>
      <TD>
         148.8 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         62.69 ns
      </TD>
      <TD CLASS = "thinright">
         2.298 μs
      </TD>
      <TD CLASS = "thinright">
         204.5 μs
      </TD>
      <TD>
         30.98 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         79.47 ns
      </TD>
      <TD CLASS = "thinright">
         6.888 μs
      </TD>
      <TD CLASS = "thinright">
         470.1 μs
      </TD>
      <TD>
         47.08 ms
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
         34.76 ns
      </TD>
      <TD CLASS = "thinright">
         996.5 ns
      </TD>
      <TD CLASS = "thinright">
         13.98 μs
      </TD>
      <TD>
         171.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         63.00 ns
      </TD>
      <TD CLASS = "thinright">
         903.4 ns
      </TD>
      <TD CLASS = "thinright">
         9.219 μs
      </TD>
      <TD>
         93.94 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         79.46 ns
      </TD>
      <TD CLASS = "thinright">
         2.360 μs
      </TD>
      <TD CLASS = "thinright">
         21.18 μs
      </TD>
      <TD>
         244.4 μs
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
         3.349 ms
      </TD>
      <TD CLASS = "thinright">
         1.664 μs
      </TD>
      <TD CLASS = "thinright">
         31.65 μs
      </TD>
      <TD>
         287.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.293 ms
      </TD>
      <TD CLASS = "thinright">
         1.323 μs
      </TD>
      <TD CLASS = "thinright">
         20.65 μs
      </TD>
      <TD>
         143.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.681 ms
      </TD>
      <TD CLASS = "thinright">
         3.448 μs
      </TD>
      <TD CLASS = "thinright">
         45.16 μs
      </TD>
      <TD>
         321.7 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 19 times
 * Alga was the fastest 4 times

 There was 1 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Fgl was 2.88 times faster than Alga
 * Hash-Graph was 1.59 times faster than Alga

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
         17.87 μs
      </TD>
      <TD CLASS = "thinright">
         7.802 ms
      </TD>
      <TD>
         1.241 s
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         4.491 μs
      </TD>
      <TD CLASS = "thinright">
         229.3 μs
      </TD>
      <TD>
         31.56 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         3.709 μs
      </TD>
      <TD CLASS = "thinright">
         354.2 μs
      </TD>
      <TD>
         40.23 ms
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
         5.175 μs
      </TD>
      <TD CLASS = "thinright">
         73.63 μs
      </TD>
      <TD>
         1.961 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.515 μs
      </TD>
      <TD CLASS = "thinright">
         10.19 μs
      </TD>
      <TD>
         94.08 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1.500 μs
      </TD>
      <TD CLASS = "thinright">
         19.31 μs
      </TD>
      <TD>
         227.9 μs
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
         47.69 ms
      </TD>
      <TD CLASS = "thinright">
         8.170 μs
      </TD>
      <TD CLASS = "thinright">
         177.2 μs
      </TD>
      <TD>
         4.838 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.326 ms
      </TD>
      <TD CLASS = "thinright">
         2.005 μs
      </TD>
      <TD CLASS = "thinright">
         23.89 μs
      </TD>
      <TD>
         156.9 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.377 ms
      </TD>
      <TD CLASS = "thinright">
         2.279 μs
      </TD>
      <TD CLASS = "thinright">
         35.50 μs
      </TD>
      <TD>
         281.0 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 24 times
 * Hash-Graph was the fastest 3 times

 There was 3 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Fgl was 30.37 times faster than Alga
 * Hash-Graph was 18.90 times faster than Alga

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
         108.8 ns
      </TD>
      <TD CLASS = "thinright">
         1.903 μs
      </TD>
      <TD CLASS = "thinright">
         182.7 μs
      </TD>
      <TD>
         28.05 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         129.3 ns
      </TD>
      <TD CLASS = "thinright">
         3.462 μs
      </TD>
      <TD CLASS = "thinright">
         350.9 μs
      </TD>
      <TD>
         39.58 ms
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
         108.7 ns
      </TD>
      <TD CLASS = "thinright">
         883.5 ns
      </TD>
      <TD CLASS = "thinright">
         9.338 μs
      </TD>
      <TD>
         91.67 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         131.9 ns
      </TD>
      <TD CLASS = "thinright">
         1.497 μs
      </TD>
      <TD CLASS = "thinright">
         18.41 μs
      </TD>
      <TD>
         226.5 μs
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
         1.272 ms
      </TD>
      <TD CLASS = "thinright">
         1.340 μs
      </TD>
      <TD CLASS = "thinright">
         19.33 μs
      </TD>
      <TD>
         142.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.325 ms
      </TD>
      <TD CLASS = "thinright">
         2.271 μs
      </TD>
      <TD CLASS = "thinright">
         35.37 μs
      </TD>
      <TD>
         271.6 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 34 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Fgl was 1.83 times faster than Hash-Graph

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
         19.02 ns
      </TD>
      <TD CLASS = "thinright">
         3.513 μs
      </TD>
      <TD CLASS = "thinright">
         405.3 μs
      </TD>
      <TD>
         38.40 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         52.07 ns
      </TD>
      <TD CLASS = "thinright">
         1.301 μs
      </TD>
      <TD CLASS = "thinright">
         151.6 μs
      </TD>
      <TD>
         76.09 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         40.67 ns
      </TD>
      <TD CLASS = "thinright">
         18.42 μs
      </TD>
      <TD CLASS = "thinright">
         13.43 ms
      </TD>
      <TD>
         3.472 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         29.34 ns
      </TD>
      <TD CLASS = "thinright">
         22.15 μs
      </TD>
      <TD CLASS = "thinright">
         6.641 ms
      </TD>
      <TD>
         3.337 s
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
         18.00 ns
      </TD>
      <TD CLASS = "thinright">
         1.021 μs
      </TD>
      <TD CLASS = "thinright">
         13.73 μs
      </TD>
      <TD>
         143.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         51.77 ns
      </TD>
      <TD CLASS = "thinright">
         539.4 ns
      </TD>
      <TD CLASS = "thinright">
         6.321 μs
      </TD>
      <TD>
         80.92 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         40.89 ns
      </TD>
      <TD CLASS = "thinright">
         5.988 μs
      </TD>
      <TD CLASS = "thinright">
         215.7 μs
      </TD>
      <TD>
         17.99 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         29.43 ns
      </TD>
      <TD CLASS = "thinright">
         6.255 μs
      </TD>
      <TD CLASS = "thinright">
         210.8 μs
      </TD>
      <TD>
         18.95 ms
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
         2.121 ms
      </TD>
      <TD CLASS = "thinright">
         1.542 μs
      </TD>
      <TD CLASS = "thinright">
         29.88 μs
      </TD>
      <TD>
         260.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         1.321 ms
      </TD>
      <TD CLASS = "thinright">
         726.9 ns
      </TD>
      <TD CLASS = "thinright">
         13.52 μs
      </TD>
      <TD>
         118.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         549.9 ms
      </TD>
      <TD CLASS = "thinright">
         10.49 μs
      </TD>
      <TD CLASS = "thinright">
         565.8 μs
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
         544.9 ms
      </TD>
      <TD CLASS = "thinright">
         11.32 μs
      </TD>
      <TD CLASS = "thinright">
         557.3 μs
      </TD>
      <TD>
         18.13 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 9 times
 * Alga was the fastest 3 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 91.84 times faster than Fgl
 * Containers was 91.79 times faster than Fgl
 * Hash-Graph was 1.03 times faster than Fgl

