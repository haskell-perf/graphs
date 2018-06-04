# Compare benchmarks

Doing:

----
* isEmpty
* vertexList
* vertexCount
* edgeCount
* edgeList
* hasEdge
* addEdge
* addVertex
* removeVertex
* removeEdge
* transpose
* dff
* topSort
* reachable
* mergeContext
* creation

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
         23.68 ns
      </TD>
      <TD CLASS = "thinright">
         29.41 ns
      </TD>
      <TD CLASS = "thinright">
         30.67 ns
      </TD>
      <TD>
         29.58 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         19.37 ns
      </TD>
      <TD CLASS = "thinright">
         20.62 ns
      </TD>
      <TD CLASS = "thinright">
         20.29 ns
      </TD>
      <TD>
         19.99 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         20.61 ns
      </TD>
      <TD CLASS = "thinright">
         19.43 ns
      </TD>
      <TD CLASS = "thinright">
         20.03 ns
      </TD>
      <TD>
         19.13 ns
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
         21.96 ns
      </TD>
      <TD CLASS = "thinright">
         29.86 ns
      </TD>
      <TD CLASS = "thinright">
         30.13 ns
      </TD>
      <TD>
         30.89 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         19.78 ns
      </TD>
      <TD CLASS = "thinright">
         20.61 ns
      </TD>
      <TD CLASS = "thinright">
         20.23 ns
      </TD>
      <TD>
         20.48 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         19.40 ns
      </TD>
      <TD CLASS = "thinright">
         20.80 ns
      </TD>
      <TD CLASS = "thinright">
         21.01 ns
      </TD>
      <TD>
         20.11 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 8 ex-aequo


ABSTRACT:

 * Hash-Graph was 1.41 times faster than Alga
 * Fgl was 1.40 times faster than Alga

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
         47.90 ns
      </TD>
      <TD CLASS = "thinright">
         1.748 μs
      </TD>
      <TD CLASS = "thinright">
         46.80 μs
      </TD>
      <TD>
         1.035 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         32.28 ns
      </TD>
      <TD CLASS = "thinright">
         148.5 ns
      </TD>
      <TD CLASS = "thinright">
         1.065 μs
      </TD>
      <TD>
         10.51 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         51.75 ns
      </TD>
      <TD CLASS = "thinright">
         412.0 ns
      </TD>
      <TD CLASS = "thinright">
         4.358 μs
      </TD>
      <TD>
         44.86 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.22 ns
      </TD>
      <TD CLASS = "thinright">
         191.6 ns
      </TD>
      <TD CLASS = "thinright">
         2.224 μs
      </TD>
      <TD>
         23.34 μs
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
         48.22 ns
      </TD>
      <TD CLASS = "thinright">
         5.102 μs
      </TD>
      <TD CLASS = "thinright">
         1.556 ms
      </TD>
      <TD>
         739.7 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         32.28 ns
      </TD>
      <TD CLASS = "thinright">
         148.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.091 μs
      </TD>
      <TD>
         10.61 μs
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
         415.7 ns
      </TD>
      <TD CLASS = "thinright">
         4.246 μs
      </TD>
      <TD>
         47.23 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.57 ns
      </TD>
      <TD CLASS = "thinright">
         197.8 ns
      </TD>
      <TD CLASS = "thinright">
         2.238 μs
      </TD>
      <TD>
         24.04 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 6 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:

 * Containers was 8914.65 times faster than Alga
 * Hash-Graph was 3947.00 times faster than Alga
 * Fgl was 2009.95 times faster than Alga

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
         67.71 ns
      </TD>
      <TD CLASS = "thinright">
         3.034 μs
      </TD>
      <TD CLASS = "thinright">
         81.34 μs
      </TD>
      <TD>
         1.677 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         20.40 ns
      </TD>
      <TD CLASS = "thinright">
         19.62 ns
      </TD>
      <TD CLASS = "thinright">
         20.26 ns
      </TD>
      <TD>
         19.31 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         21.82 ns
      </TD>
      <TD CLASS = "thinright">
         100.2 ns
      </TD>
      <TD CLASS = "thinright">
         845.1 ns
      </TD>
      <TD>
         8.660 μs
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
         89.03 ns
      </TD>
      <TD CLASS = "thinright">
         761.5 ns
      </TD>
      <TD>
         7.992 μs
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
         67.51 ns
      </TD>
      <TD CLASS = "thinright">
         9.328 μs
      </TD>
      <TD CLASS = "thinright">
         2.593 ms
      </TD>
      <TD>
         967.5 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         19.33 ns
      </TD>
      <TD CLASS = "thinright">
         19.59 ns
      </TD>
      <TD CLASS = "thinright">
         19.15 ns
      </TD>
      <TD>
         19.13 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         21.23 ns
      </TD>
      <TD CLASS = "thinright">
         99.40 ns
      </TD>
      <TD CLASS = "thinright">
         863.5 ns
      </TD>
      <TD>
         9.353 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         23.05 ns
      </TD>
      <TD CLASS = "thinright">
         85.91 ns
      </TD>
      <TD CLASS = "thinright">
         752.6 ns
      </TD>
      <TD>
         7.837 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 6 times
There was 2 ex-aequo


ABSTRACT:

 * Containers was 6350546.25 times faster than Alga
 * Hash-Graph was 15919.91 times faster than Alga
 * Fgl was 13358.26 times faster than Alga

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
         62.00 ns
      </TD>
      <TD CLASS = "thinright">
         9.543 μs
      </TD>
      <TD CLASS = "thinright">
         194.2 μs
      </TD>
      <TD>
         3.698 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         30.08 ns
      </TD>
      <TD CLASS = "thinright">
         134.4 ns
      </TD>
      <TD CLASS = "thinright">
         1.266 μs
      </TD>
      <TD>
         12.92 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         34.98 ns
      </TD>
      <TD CLASS = "thinright">
         695.8 ns
      </TD>
      <TD CLASS = "thinright">
         9.087 μs
      </TD>
      <TD>
         97.88 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         37.37 ns
      </TD>
      <TD CLASS = "thinright">
         675.3 ns
      </TD>
      <TD CLASS = "thinright">
         8.172 μs
      </TD>
      <TD>
         106.4 μs
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
         61.92 ns
      </TD>
      <TD CLASS = "thinright">
         36.00 μs
      </TD>
      <TD CLASS = "thinright">
         9.097 ms
      </TD>
      <TD>
         1.357 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         29.18 ns
      </TD>
      <TD CLASS = "thinright">
         253.7 ns
      </TD>
      <TD CLASS = "thinright">
         25.87 μs
      </TD>
      <TD>
         24.19 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         34.81 ns
      </TD>
      <TD CLASS = "thinright">
         2.023 μs
      </TD>
      <TD CLASS = "thinright">
         206.4 μs
      </TD>
      <TD>
         105.5 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         40.88 ns
      </TD>
      <TD CLASS = "thinright">
         899.6 ns
      </TD>
      <TD CLASS = "thinright">
         52.48 μs
      </TD>
      <TD>
         6.414 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 7 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:

 * Containers was 133.06 times faster than Alga
 * Hash-Graph was 62.59 times faster than Alga
 * Fgl was 18.89 times faster than Alga

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
         56.00 ns
      </TD>
      <TD CLASS = "thinright">
         10.08 μs
      </TD>
      <TD CLASS = "thinright">
         200.6 μs
      </TD>
      <TD>
         3.788 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         38.37 ns
      </TD>
      <TD CLASS = "thinright">
         417.9 ns
      </TD>
      <TD CLASS = "thinright">
         4.641 μs
      </TD>
      <TD>
         48.46 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         34.64 ns
      </TD>
      <TD CLASS = "thinright">
         970.1 ns
      </TD>
      <TD CLASS = "thinright">
         13.47 μs
      </TD>
      <TD>
         146.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         33.86 ns
      </TD>
      <TD CLASS = "thinright">
         697.3 ns
      </TD>
      <TD CLASS = "thinright">
         9.970 μs
      </TD>
      <TD>
         150.5 μs
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
         58.88 ns
      </TD>
      <TD CLASS = "thinright">
         37.14 μs
      </TD>
      <TD CLASS = "thinright">
         9.119 ms
      </TD>
      <TD>
         1.318 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         37.72 ns
      </TD>
      <TD CLASS = "thinright">
         1.011 μs
      </TD>
      <TD CLASS = "thinright">
         90.58 μs
      </TD>
      <TD>
         25.73 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         33.56 ns
      </TD>
      <TD CLASS = "thinright">
         2.943 μs
      </TD>
      <TD CLASS = "thinright">
         355.5 μs
      </TD>
      <TD>
         118.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         33.46 ns
      </TD>
      <TD CLASS = "thinright">
         1.854 μs
      </TD>
      <TD CLASS = "thinright">
         313.9 μs
      </TD>
      <TD>
         228.4 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 6 times
There was 2 ex-aequo

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
         655.5 ns
      </TD>
      <TD CLASS = "thinright">
         8.040 μs
      </TD>
      <TD CLASS = "thinright">
         80.06 μs
      </TD>
      <TD>
         992.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         27.35 ns
      </TD>
      <TD CLASS = "thinright">
         48.05 ns
      </TD>
      <TD CLASS = "thinright">
         48.96 ns
      </TD>
      <TD>
         49.45 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         111.0 ns
      </TD>
      <TD CLASS = "thinright">
         418.9 ns
      </TD>
      <TD CLASS = "thinright">
         526.8 ns
      </TD>
      <TD>
         648.5 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         99.97 ns
      </TD>
      <TD CLASS = "thinright">
         144.5 ns
      </TD>
      <TD CLASS = "thinright">
         156.8 ns
      </TD>
      <TD>
         168.5 ns
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
         664.1 ns
      </TD>
      <TD CLASS = "thinright">
         24.94 μs
      </TD>
      <TD CLASS = "thinright">
         3.112 ms
      </TD>
      <TD>
         326.3 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         26.54 ns
      </TD>
      <TD CLASS = "thinright">
         136.3 ns
      </TD>
      <TD CLASS = "thinright">
         1.268 μs
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
         111.8 ns
      </TD>
      <TD CLASS = "thinright">
         1.224 μs
      </TD>
      <TD CLASS = "thinright">
         18.93 μs
      </TD>
      <TD>
         455.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         99.70 ns
      </TD>
      <TD CLASS = "thinright">
         145.2 ns
      </TD>
      <TD CLASS = "thinright">
         162.4 ns
      </TD>
      <TD>
         174.5 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 16 times
 * Hash-Graph was the fastest 10 times


ABSTRACT:

 * Hash-Graph was 304119.51 times faster than Alga
 * Containers was 7344.65 times faster than Alga
 * Fgl was 407.31 times faster than Alga

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
         59.98 ns
      </TD>
      <TD CLASS = "thinright">
         365.5 ns
      </TD>
      <TD CLASS = "thinright">
         4.282 μs
      </TD>
      <TD>
         45.25 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         120.4 ns
      </TD>
      <TD CLASS = "thinright">
         1.031 μs
      </TD>
      <TD CLASS = "thinright">
         10.12 μs
      </TD>
      <TD>
         103.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         153.4 ns
      </TD>
      <TD CLASS = "thinright">
         1.755 μs
      </TD>
      <TD CLASS = "thinright">
         19.12 μs
      </TD>
      <TD>
         222.7 μs
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
         58.52 ns
      </TD>
      <TD CLASS = "thinright">
         1.101 μs
      </TD>
      <TD CLASS = "thinright">
         115.1 μs
      </TD>
      <TD>
         16.16 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         119.8 ns
      </TD>
      <TD CLASS = "thinright">
         2.280 μs
      </TD>
      <TD CLASS = "thinright">
         235.7 μs
      </TD>
      <TD>
         33.68 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         153.6 ns
      </TD>
      <TD CLASS = "thinright">
         3.809 μs
      </TD>
      <TD CLASS = "thinright">
         329.2 μs
      </TD>
      <TD>
         39.06 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 26 times


ABSTRACT:

 * Alga was 3.46 times faster than Hash-Graph
 * Fgl was 1.55 times faster than Hash-Graph

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
         38.28 ns
      </TD>
      <TD CLASS = "thinright">
         342.1 ns
      </TD>
      <TD CLASS = "thinright">
         4.220 μs
      </TD>
      <TD>
         44.44 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         78.63 ns
      </TD>
      <TD CLASS = "thinright">
         814.9 ns
      </TD>
      <TD CLASS = "thinright">
         9.366 μs
      </TD>
      <TD>
         98.83 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         102.6 ns
      </TD>
      <TD CLASS = "thinright">
         1.384 μs
      </TD>
      <TD CLASS = "thinright">
         18.54 μs
      </TD>
      <TD>
         236.3 μs
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
         37.62 ns
      </TD>
      <TD CLASS = "thinright">
         1.063 μs
      </TD>
      <TD CLASS = "thinright">
         114.8 μs
      </TD>
      <TD>
         15.13 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         80.16 ns
      </TD>
      <TD CLASS = "thinright">
         2.001 μs
      </TD>
      <TD CLASS = "thinright">
         205.4 μs
      </TD>
      <TD>
         33.27 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         107.0 ns
      </TD>
      <TD CLASS = "thinright">
         3.548 μs
      </TD>
      <TD CLASS = "thinright">
         349.6 μs
      </TD>
      <TD>
         43.75 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 8 times


ABSTRACT:

 * Alga was 3.39 times faster than Hash-Graph
 * Fgl was 1.62 times faster than Hash-Graph

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
         35.75 ns
      </TD>
      <TD CLASS = "thinright">
         1.177 μs
      </TD>
      <TD CLASS = "thinright">
         15.36 μs
      </TD>
      <TD>
         166.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         43.72 ns
      </TD>
      <TD CLASS = "thinright">
         906.3 ns
      </TD>
      <TD CLASS = "thinright">
         9.478 μs
      </TD>
      <TD>
         95.70 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         52.55 ns
      </TD>
      <TD CLASS = "thinright">
         2.230 μs
      </TD>
      <TD CLASS = "thinright">
         20.55 μs
      </TD>
      <TD>
         244.2 μs
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
         36.77 ns
      </TD>
      <TD CLASS = "thinright">
         3.976 μs
      </TD>
      <TD CLASS = "thinright">
         460.5 μs
      </TD>
      <TD>
         44.59 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         45.39 ns
      </TD>
      <TD CLASS = "thinright">
         2.309 μs
      </TD>
      <TD CLASS = "thinright">
         222.6 μs
      </TD>
      <TD>
         31.15 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         51.88 ns
      </TD>
      <TD CLASS = "thinright">
         6.530 μs
      </TD>
      <TD CLASS = "thinright">
         417.4 μs
      </TD>
      <TD>
         44.49 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 6 times
 * Alga was the fastest 2 times


ABSTRACT:

 * Fgl was 1.90 times faster than Hash-Graph
 * Alga was 1.32 times faster than Hash-Graph

ABSTRACT:

 * Containers was 42.15 times faster than Alga
 * Hash-Graph was 14.75 times faster than Alga
 * Fgl was 12.99 times faster than Alga

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
         5.298 μs
      </TD>
      <TD CLASS = "thinright">
         71.81 μs
      </TD>
      <TD>
         1.879 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.478 μs
      </TD>
      <TD CLASS = "thinright">
         10.22 μs
      </TD>
      <TD>
         94.81 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1.430 μs
      </TD>
      <TD CLASS = "thinright">
         16.85 μs
      </TD>
      <TD>
         204.0 μs
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
         17.40 μs
      </TD>
      <TD CLASS = "thinright">
         7.691 ms
      </TD>
      <TD>
         1.070 s
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         4.381 μs
      </TD>
      <TD CLASS = "thinright">
         230.6 μs
      </TD>
      <TD>
         33.68 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         3.527 μs
      </TD>
      <TD CLASS = "thinright">
         324.1 μs
      </TD>
      <TD>
         37.65 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 12 times
 * Hash-Graph was the fastest 4 times
There was 2 ex-aequo


ABSTRACT:

 * Fgl was 16.59 times faster than Alga
 * Hash-Graph was 12.37 times faster than Alga

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
         27.02 ns
      </TD>
      <TD CLASS = "thinright">
         769.5 ns
      </TD>
      <TD CLASS = "thinright">
         10.75 μs
      </TD>
      <TD>
         114.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         90.67 ns
      </TD>
      <TD CLASS = "thinright">
         903.6 ns
      </TD>
      <TD CLASS = "thinright">
         10.01 μs
      </TD>
      <TD>
         120.1 μs
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
         27.45 ns
      </TD>
      <TD CLASS = "thinright">
         2.706 μs
      </TD>
      <TD CLASS = "thinright">
         297.8 μs
      </TD>
      <TD>
         28.54 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         91.75 ns
      </TD>
      <TD CLASS = "thinright">
         2.289 μs
      </TD>
      <TD CLASS = "thinright">
         277.8 μs
      </TD>
      <TD>
         77.08 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 4 times
 * Containers was the fastest 1 times
There was 3 ex-aequo


ABSTRACT:

 * Alga was 1.31 times faster than Containers

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
         124.8 ns
      </TD>
      <TD CLASS = "thinright">
         1.405 μs
      </TD>
      <TD CLASS = "thinright">
         14.35 μs
      </TD>
      <TD>
         160.9 μs
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
         4.319 μs
      </TD>
      <TD CLASS = "thinright">
         68.50 μs
      </TD>
      <TD>
         902.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         64.75 ns
      </TD>
      <TD CLASS = "thinright">
         2.601 μs
      </TD>
      <TD CLASS = "thinright">
         49.77 μs
      </TD>
      <TD>
         1.662 ms
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
         122.1 ns
      </TD>
      <TD CLASS = "thinright">
         2.419 μs
      </TD>
      <TD CLASS = "thinright">
         171.3 μs
      </TD>
      <TD>
         32.25 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         208.5 ns
      </TD>
      <TD CLASS = "thinright">
         5.713 μs
      </TD>
      <TD CLASS = "thinright">
         561.3 μs
      </TD>
      <TD>
         119.5 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         64.42 ns
      </TD>
      <TD CLASS = "thinright">
         5.581 μs
      </TD>
      <TD CLASS = "thinright">
         558.4 μs
      </TD>
      <TD>
         104.5 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 6 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:

 * Containers was 2.74 times faster than Fgl
 * Hash-Graph was 1.99 times faster than Fgl

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
         140.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.554 μs
      </TD>
      <TD CLASS = "thinright">
         15.47 μs
      </TD>
      <TD>
         178.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         339.5 ns
      </TD>
      <TD CLASS = "thinright">
         6.850 μs
      </TD>
      <TD CLASS = "thinright">
         111.4 μs
      </TD>
      <TD>
         2.657 ms
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
         141.0 ns
      </TD>
      <TD CLASS = "thinright">
         2.498 μs
      </TD>
      <TD CLASS = "thinright">
         173.6 μs
      </TD>
      <TD>
         28.26 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         343.2 ns
      </TD>
      <TD CLASS = "thinright">
         9.157 μs
      </TD>
      <TD CLASS = "thinright">
         805.3 μs
      </TD>
      <TD>
         141.4 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 8 times


ABSTRACT:

 * Containers was 4.11 times faster than Fgl

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
         136.4 ns
      </TD>
      <TD CLASS = "thinright">
         1.079 μs
      </TD>
      <TD CLASS = "thinright">
         10.78 μs
      </TD>
      <TD>
         120.3 μs
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
         5.828 μs
      </TD>
      <TD CLASS = "thinright">
         83.01 μs
      </TD>
      <TD>
         1.282 ms
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
         127.0 ns
      </TD>
      <TD CLASS = "thinright">
         2.064 μs
      </TD>
      <TD CLASS = "thinright">
         169.2 μs
      </TD>
      <TD>
         26.35 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         268.8 ns
      </TD>
      <TD CLASS = "thinright">
         7.591 μs
      </TD>
      <TD CLASS = "thinright">
         614.0 μs
      </TD>
      <TD>
         120.9 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 8 times


ABSTRACT:

 * Containers was 3.72 times faster than Fgl

## mergeContext

Descritpion: Merge an FGL context in the graph

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
         91.31 ns
      </TD>
      <TD CLASS = "thinright">
         900.6 ns
      </TD>
      <TD CLASS = "thinright">
         9.417 μs
      </TD>
      <TD>
         92.04 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         75.16 ns
      </TD>
      <TD CLASS = "thinright">
         1.314 μs
      </TD>
      <TD CLASS = "thinright">
         16.77 μs
      </TD>
      <TD>
         215.5 μs
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
         93.34 ns
      </TD>
      <TD CLASS = "thinright">
         1.908 μs
      </TD>
      <TD CLASS = "thinright">
         185.5 μs
      </TD>
      <TD>
         28.26 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         75.35 ns
      </TD>
      <TD CLASS = "thinright">
         3.140 μs
      </TD>
      <TD CLASS = "thinright">
         325.5 μs
      </TD>
      <TD>
         36.61 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 6 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:

 * Fgl was 1.32 times faster than Hash-Graph

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
         26.05 ns
      </TD>
      <TD CLASS = "thinright">
         1.893 μs
      </TD>
      <TD CLASS = "thinright">
         26.86 μs
      </TD>
      <TD>
         290.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         64.49 ns
      </TD>
      <TD CLASS = "thinright">
         525.6 ns
      </TD>
      <TD CLASS = "thinright">
         6.530 μs
      </TD>
      <TD>
         72.01 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         37.43 ns
      </TD>
      <TD CLASS = "thinright">
         5.812 μs
      </TD>
      <TD CLASS = "thinright">
         202.5 μs
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
         27.07 ns
      </TD>
      <TD CLASS = "thinright">
         6.542 μs
      </TD>
      <TD CLASS = "thinright">
         209.0 μs
      </TD>
      <TD>
         18.29 ms
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
         25.82 ns
      </TD>
      <TD CLASS = "thinright">
         6.982 μs
      </TD>
      <TD CLASS = "thinright">
         800.3 μs
      </TD>
      <TD>
         75.43 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         61.57 ns
      </TD>
      <TD CLASS = "thinright">
         1.272 μs
      </TD>
      <TD CLASS = "thinright">
         121.2 μs
      </TD>
      <TD>
         78.72 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         37.12 ns
      </TD>
      <TD CLASS = "thinright">
         18.16 μs
      </TD>
      <TD CLASS = "thinright">
         9.142 ms
      </TD>
      <TD>
         3.367 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         26.57 ns
      </TD>
      <TD CLASS = "thinright">
         21.93 μs
      </TD>
      <TD CLASS = "thinright">
         7.525 ms
      </TD>
      <TD>
         3.691 s
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 5 times
There was 3 ex-aequo


ABSTRACT:

 * Containers was 10.85 times faster than Fgl
 * Alga was 3.39 times faster than Fgl
 * Hash-Graph was 1.19 times faster than Fgl
