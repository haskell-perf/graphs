The content of this file was obtained with:
```Bash
$ time run -g '("Clique",4)' -g '("Mesh",4)' -g '("RealLife",4)' -d Html
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
         47.14 ns
      </TD>
      <TD CLASS = "thinright">
         38.58 μs
      </TD>
      <TD CLASS = "thinright">
         7.614 ms
      </TD>
      <TD>
         1.147 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         37.79 ns
      </TD>
      <TD CLASS = "thinright">
         961.2 ns
      </TD>
      <TD CLASS = "thinright">
         80.39 μs
      </TD>
      <TD>
         25.33 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         32.17 ns
      </TD>
      <TD CLASS = "thinright">
         2.882 μs
      </TD>
      <TD CLASS = "thinright">
         327.3 μs
      </TD>
      <TD>
         120.8 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         35.29 ns
      </TD>
      <TD CLASS = "thinright">
         1.808 μs
      </TD>
      <TD CLASS = "thinright">
         304.7 μs
      </TD>
      <TD>
         228.5 ms
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
         46.24 ns
      </TD>
      <TD CLASS = "thinright">
         10.08 μs
      </TD>
      <TD CLASS = "thinright">
         200.6 μs
      </TD>
      <TD>
         3.350 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         37.88 ns
      </TD>
      <TD CLASS = "thinright">
         446.6 ns
      </TD>
      <TD CLASS = "thinright">
         4.564 μs
      </TD>
      <TD>
         49.38 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         32.46 ns
      </TD>
      <TD CLASS = "thinright">
         926.8 ns
      </TD>
      <TD CLASS = "thinright">
         12.44 μs
      </TD>
      <TD>
         136.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         35.27 ns
      </TD>
      <TD CLASS = "thinright">
         686.2 ns
      </TD>
      <TD CLASS = "thinright">
         9.763 μs
      </TD>
      <TD>
         148.5 μs
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
         17.50 μs
      </TD>
      <TD CLASS = "thinright">
         521.7 μs
      </TD>
      <TD CLASS = "thinright">
         6.392 ms
      </TD>
      <TD>
         79.88 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         606.5 ns
      </TD>
      <TD CLASS = "thinright">
         8.157 μs
      </TD>
      <TD CLASS = "thinright">
         58.55 μs
      </TD>
      <TD>
         451.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.433 μs
      </TD>
      <TD CLASS = "thinright">
         26.42 μs
      </TD>
      <TD CLASS = "thinright">
         207.7 μs
      </TD>
      <TD>
         4.662 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1.025 μs
      </TD>
      <TD CLASS = "thinright">
         20.02 μs
      </TD>
      <TD CLASS = "thinright">
         203.4 μs
      </TD>
      <TD>
         7.747 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 70.64 times faster than Alga
 * Fgl was 15.72 times faster than Alga
 * Hash-Graph was 11.81 times faster than Alga

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
         43.69 ns
      </TD>
      <TD CLASS = "thinright">
         16.95 μs
      </TD>
      <TD CLASS = "thinright">
         3.694 ms
      </TD>
      <TD>
         568.5 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         32.21 ns
      </TD>
      <TD CLASS = "thinright">
         151.2 ns
      </TD>
      <TD CLASS = "thinright">
         1.079 μs
      </TD>
      <TD>
         10.50 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         57.28 ns
      </TD>
      <TD CLASS = "thinright">
         418.7 ns
      </TD>
      <TD CLASS = "thinright">
         4.346 μs
      </TD>
      <TD>
         43.70 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         23.51 ns
      </TD>
      <TD CLASS = "thinright">
         200.8 ns
      </TD>
      <TD CLASS = "thinright">
         2.230 μs
      </TD>
      <TD>
         26.49 μs
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
         43.86 ns
      </TD>
      <TD CLASS = "thinright">
         4.890 μs
      </TD>
      <TD CLASS = "thinright">
         123.4 μs
      </TD>
      <TD>
         2.039 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         32.03 ns
      </TD>
      <TD CLASS = "thinright">
         151.3 ns
      </TD>
      <TD CLASS = "thinright">
         1.094 μs
      </TD>
      <TD>
         10.47 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         53.64 ns
      </TD>
      <TD CLASS = "thinright">
         426.4 ns
      </TD>
      <TD CLASS = "thinright">
         4.297 μs
      </TD>
      <TD>
         43.74 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         23.44 ns
      </TD>
      <TD CLASS = "thinright">
         201.3 ns
      </TD>
      <TD CLASS = "thinright">
         2.235 μs
      </TD>
      <TD>
         26.47 μs
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
         8.917 μs
      </TD>
      <TD CLASS = "thinright">
         259.8 μs
      </TD>
      <TD CLASS = "thinright">
         2.806 ms
      </TD>
      <TD>
         31.11 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         190.2 ns
      </TD>
      <TD CLASS = "thinright">
         950.9 ns
      </TD>
      <TD CLASS = "thinright">
         3.698 μs
      </TD>
      <TD>
         17.20 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         581.8 ns
      </TD>
      <TD CLASS = "thinright">
         3.829 μs
      </TD>
      <TD CLASS = "thinright">
         15.24 μs
      </TD>
      <TD>
         71.42 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         279.4 ns
      </TD>
      <TD CLASS = "thinright">
         2.008 μs
      </TD>
      <TD CLASS = "thinright">
         9.825 μs
      </TD>
      <TD>
         36.49 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 525.85 times faster than Alga
 * Hash-Graph was 221.43 times faster than Alga
 * Fgl was 126.28 times faster than Alga

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
         197.9 ns
      </TD>
      <TD CLASS = "thinright">
         59.12 μs
      </TD>
      <TD CLASS = "thinright">
         11.23 ms
      </TD>
      <TD>
         1.831 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         29.29 ns
      </TD>
      <TD CLASS = "thinright">
         265.8 ns
      </TD>
      <TD CLASS = "thinright">
         23.79 μs
      </TD>
      <TD>
         10.25 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         159.2 ns
      </TD>
      <TD CLASS = "thinright">
         14.75 μs
      </TD>
      <TD CLASS = "thinright">
         2.701 ms
      </TD>
      <TD>
         502.5 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         67.59 ns
      </TD>
      <TD CLASS = "thinright">
         4.211 μs
      </TD>
      <TD CLASS = "thinright">
         435.5 μs
      </TD>
      <TD>
         55.84 ms
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
         194.3 ns
      </TD>
      <TD CLASS = "thinright">
         16.17 μs
      </TD>
      <TD CLASS = "thinright">
         315.1 μs
      </TD>
      <TD>
         4.912 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         29.20 ns
      </TD>
      <TD CLASS = "thinright">
         121.8 ns
      </TD>
      <TD CLASS = "thinright">
         1.111 μs
      </TD>
      <TD>
         11.95 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         159.6 ns
      </TD>
      <TD CLASS = "thinright">
         5.074 μs
      </TD>
      <TD CLASS = "thinright">
         62.26 μs
      </TD>
      <TD>
         888.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         65.47 ns
      </TD>
      <TD CLASS = "thinright">
         1.715 μs
      </TD>
      <TD CLASS = "thinright">
         21.63 μs
      </TD>
      <TD>
         298.8 μs
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
         27.83 μs
      </TD>
      <TD CLASS = "thinright">
         801.1 μs
      </TD>
      <TD CLASS = "thinright">
         8.979 ms
      </TD>
      <TD>
         110.1 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         179.4 ns
      </TD>
      <TD CLASS = "thinright">
         2.506 μs
      </TD>
      <TD CLASS = "thinright">
         18.07 μs
      </TD>
      <TD>
         156.9 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         7.689 μs
      </TD>
      <TD CLASS = "thinright">
         136.0 μs
      </TD>
      <TD CLASS = "thinright">
         2.031 ms
      </TD>
      <TD>
         30.31 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.557 μs
      </TD>
      <TD CLASS = "thinright">
         39.81 μs
      </TD>
      <TD CLASS = "thinright">
         326.8 μs
      </TD>
      <TD>
         3.452 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 22 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 442.15 times faster than Alga
 * Hash-Graph was 60.95 times faster than Alga
 * Fgl was 3.01 times faster than Alga

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
         26.09 ns
      </TD>
      <TD CLASS = "thinright">
         3.086 μs
      </TD>
      <TD CLASS = "thinright">
         331.5 μs
      </TD>
      <TD>
         31.47 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         89.12 ns
      </TD>
      <TD CLASS = "thinright">
         2.111 μs
      </TD>
      <TD CLASS = "thinright">
         271.8 μs
      </TD>
      <TD>
         87.93 ms
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
         26.54 ns
      </TD>
      <TD CLASS = "thinright">
         882.5 ns
      </TD>
      <TD CLASS = "thinright">
         12.21 μs
      </TD>
      <TD>
         132.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         88.83 ns
      </TD>
      <TD CLASS = "thinright">
         924.2 ns
      </TD>
      <TD CLASS = "thinright">
         10.07 μs
      </TD>
      <TD>
         120.9 μs
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
         1.443 μs
      </TD>
      <TD CLASS = "thinright">
         27.14 μs
      </TD>
      <TD CLASS = "thinright">
         215.4 μs
      </TD>
      <TD>
         1.658 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         1.386 μs
      </TD>
      <TD CLASS = "thinright">
         21.56 μs
      </TD>
      <TD CLASS = "thinright">
         195.4 μs
      </TD>
      <TD>
         3.587 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 5 times
 * Alga was the fastest 4 times

 There was 3 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 1.96 times faster than Containers

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
         149.4 ns
      </TD>
      <TD CLASS = "thinright">
         2.670 μs
      </TD>
      <TD CLASS = "thinright">
         183.7 μs
      </TD>
      <TD>
         30.45 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         222.2 ns
      </TD>
      <TD CLASS = "thinright">
         6.115 μs
      </TD>
      <TD CLASS = "thinright">
         570.7 μs
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
         63.62 ns
      </TD>
      <TD CLASS = "thinright">
         5.771 μs
      </TD>
      <TD CLASS = "thinright">
         563.8 μs
      </TD>
      <TD>
         164.6 ms
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
         150.9 ns
      </TD>
      <TD CLASS = "thinright">
         1.440 μs
      </TD>
      <TD CLASS = "thinright">
         14.84 μs
      </TD>
      <TD>
         165.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         224.8 ns
      </TD>
      <TD CLASS = "thinright">
         4.659 μs
      </TD>
      <TD CLASS = "thinright">
         69.52 μs
      </TD>
      <TD>
         904.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         63.85 ns
      </TD>
      <TD CLASS = "thinright">
         2.612 μs
      </TD>
      <TD CLASS = "thinright">
         50.16 μs
      </TD>
      <TD>
         1.816 ms
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
         2.174 μs
      </TD>
      <TD CLASS = "thinright">
         23.36 μs
      </TD>
      <TD CLASS = "thinright">
         162.7 μs
      </TD>
      <TD>
         5.045 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         7.420 μs
      </TD>
      <TD CLASS = "thinright">
         140.7 μs
      </TD>
      <TD CLASS = "thinright">
         1.583 ms
      </TD>
      <TD>
         19.43 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         4.486 μs
      </TD>
      <TD CLASS = "thinright">
         67.85 μs
      </TD>
      <TD CLASS = "thinright">
         174.5 ns
      </TD>
      <TD>
         35.58 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 8 times
 * Hash-Graph was the fastest 4 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 204.72 times faster than Fgl
 * Containers was 4.32 times faster than Fgl

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
         143.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.558 μs
      </TD>
      <TD CLASS = "thinright">
         15.89 μs
      </TD>
      <TD>
         179.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         357.4 ns
      </TD>
      <TD CLASS = "thinright">
         7.240 μs
      </TD>
      <TD CLASS = "thinright">
         116.5 μs
      </TD>
      <TD>
         2.798 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         201.9 ns
      </TD>
      <TD CLASS = "thinright">
         5.748 μs
      </TD>
      <TD CLASS = "thinright">
         81.12 μs
      </TD>
      <TD>
         1.181 ms
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
         138.4 ns
      </TD>
      <TD CLASS = "thinright">
         2.593 μs
      </TD>
      <TD CLASS = "thinright">
         175.6 μs
      </TD>
      <TD>
         31.52 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         353.1 ns
      </TD>
      <TD CLASS = "thinright">
         9.628 μs
      </TD>
      <TD CLASS = "thinright">
         891.0 μs
      </TD>
      <TD>
         154.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         202.7 ns
      </TD>
      <TD CLASS = "thinright">
         6.956 μs
      </TD>
      <TD CLASS = "thinright">
         322.7 μs
      </TD>
      <TD>
         35.99 ms
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
         2.349 μs
      </TD>
      <TD CLASS = "thinright">
         24.21 μs
      </TD>
      <TD CLASS = "thinright">
         166.8 μs
      </TD>
      <TD>
         4.039 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         11.54 μs
      </TD>
      <TD CLASS = "thinright">
         190.4 μs
      </TD>
      <TD CLASS = "thinright">
         3.881 ms
      </TD>
      <TD>
         33.30 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         9.182 μs
      </TD>
      <TD CLASS = "thinright">
         28.58 μs
      </TD>
      <TD CLASS = "thinright">
         77.75 μs
      </TD>
      <TD>
         506.4 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 10 times
 * Hash-Graph was the fastest 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 23.10 times faster than Fgl
 * Containers was 7.70 times faster than Fgl

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
         122.8 ns
      </TD>
      <TD CLASS = "thinright">
         1.247 μs
      </TD>
      <TD CLASS = "thinright">
         93.48 μs
      </TD>
      <TD>
         14.92 ms
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
         4.592 μs
      </TD>
      <TD CLASS = "thinright">
         340.1 μs
      </TD>
      <TD>
         61.37 ms
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
         132.2 ns
      </TD>
      <TD CLASS = "thinright">
         647.7 ns
      </TD>
      <TD CLASS = "thinright">
         5.725 μs
      </TD>
      <TD>
         63.74 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         268.4 ns
      </TD>
      <TD CLASS = "thinright">
         3.297 μs
      </TD>
      <TD CLASS = "thinright">
         43.32 μs
      </TD>
      <TD>
         724.9 μs
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
         895.6 ns
      </TD>
      <TD CLASS = "thinright">
         18.16 μs
      </TD>
      <TD CLASS = "thinright">
         69.11 μs
      </TD>
      <TD>
         1.204 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         4.869 μs
      </TD>
      <TD CLASS = "thinright">
         146.7 μs
      </TD>
      <TD CLASS = "thinright">
         1.066 ms
      </TD>
      <TD>
         12.62 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 21 times
 * Fgl was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 2.61 times faster than Fgl

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
         52.63 ns
      </TD>
      <TD CLASS = "thinright">
         38.65 μs
      </TD>
      <TD CLASS = "thinright">
         7.871 ms
      </TD>
      <TD>
         1.164 s
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         28.97 ns
      </TD>
      <TD CLASS = "thinright">
         275.5 ns
      </TD>
      <TD CLASS = "thinright">
         26.48 μs
      </TD>
      <TD>
         22.55 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         33.68 ns
      </TD>
      <TD CLASS = "thinright">
         1.917 μs
      </TD>
      <TD CLASS = "thinright">
         201.3 μs
      </TD>
      <TD>
         110.5 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         38.25 ns
      </TD>
      <TD CLASS = "thinright">
         968.3 ns
      </TD>
      <TD CLASS = "thinright">
         56.22 μs
      </TD>
      <TD>
         6.186 ms
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
         52.53 ns
      </TD>
      <TD CLASS = "thinright">
         9.954 μs
      </TD>
      <TD CLASS = "thinright">
         200.1 μs
      </TD>
      <TD>
         3.169 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         28.19 ns
      </TD>
      <TD CLASS = "thinright">
         156.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.174 μs
      </TD>
      <TD>
         12.31 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         34.05 ns
      </TD>
      <TD CLASS = "thinright">
         683.1 ns
      </TD>
      <TD CLASS = "thinright">
         8.969 μs
      </TD>
      <TD>
         96.94 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         38.35 ns
      </TD>
      <TD CLASS = "thinright">
         717.7 ns
      </TD>
      <TD CLASS = "thinright">
         8.511 μs
      </TD>
      <TD>
         111.3 μs
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
         17.12 μs
      </TD>
      <TD CLASS = "thinright">
         519.0 μs
      </TD>
      <TD CLASS = "thinright">
         5.838 ms
      </TD>
      <TD>
         77.29 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         212.2 ns
      </TD>
      <TD CLASS = "thinright">
         2.207 μs
      </TD>
      <TD CLASS = "thinright">
         18.15 μs
      </TD>
      <TD>
         134.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.008 μs
      </TD>
      <TD CLASS = "thinright">
         18.44 μs
      </TD>
      <TD CLASS = "thinright">
         141.1 μs
      </TD>
      <TD>
         3.275 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1.058 μs
      </TD>
      <TD CLASS = "thinright">
         11.55 μs
      </TD>
      <TD CLASS = "thinright">
         74.92 μs
      </TD>
      <TD>
         522.6 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 11 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 160.51 times faster than Alga
 * Containers was 120.03 times faster than Alga
 * Fgl was 14.88 times faster than Alga

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
         684.9 ns
      </TD>
      <TD CLASS = "thinright">
         8.835 μs
      </TD>
      <TD CLASS = "thinright">
         633.4 μs
      </TD>
      <TD>
         87.70 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         26.87 ns
      </TD>
      <TD CLASS = "thinright">
         105.9 ns
      </TD>
      <TD CLASS = "thinright">
         912.4 ns
      </TD>
      <TD>
         9.293 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         117.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.315 μs
      </TD>
      <TD CLASS = "thinright">
         18.30 μs
      </TD>
      <TD>
         454.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         97.66 ns
      </TD>
      <TD CLASS = "thinright">
         140.3 ns
      </TD>
      <TD CLASS = "thinright">
         155.7 ns
      </TD>
      <TD>
         170.5 ns
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
         690.6 ns
      </TD>
      <TD CLASS = "thinright">
         3.492 μs
      </TD>
      <TD CLASS = "thinright">
         24.63 μs
      </TD>
      <TD>
         246.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         27.19 ns
      </TD>
      <TD CLASS = "thinright">
         46.06 ns
      </TD>
      <TD CLASS = "thinright">
         47.70 ns
      </TD>
      <TD>
         45.96 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         116.5 ns
      </TD>
      <TD CLASS = "thinright">
         452.0 ns
      </TD>
      <TD CLASS = "thinright">
         568.6 ns
      </TD>
      <TD>
         710.4 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         101.5 ns
      </TD>
      <TD CLASS = "thinright">
         138.1 ns
      </TD>
      <TD CLASS = "thinright">
         144.7 ns
      </TD>
      <TD>
         154.0 ns
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
         4.543 μs
      </TD>
      <TD CLASS = "thinright">
         54.58 μs
      </TD>
      <TD CLASS = "thinright">
         413.7 μs
      </TD>
      <TD>
         3.298 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         53.55 ns
      </TD>
      <TD CLASS = "thinright">
         45.81 ns
      </TD>
      <TD CLASS = "thinright">
         84.25 ns
      </TD>
      <TD>
         149.5 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         479.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.207 μs
      </TD>
      <TD CLASS = "thinright">
         6.100 μs
      </TD>
      <TD>
         8.765 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         133.2 ns
      </TD>
      <TD CLASS = "thinright">
         143.2 ns
      </TD>
      <TD CLASS = "thinright">
         156.9 ns
      </TD>
      <TD>
         158.9 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 46 times
 * Hash-Graph was the fastest 14 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 180132.41 times faster than Alga
 * Containers was 8859.28 times faster than Alga
 * Fgl was 329.42 times faster than Alga

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
         20.11 ns
      </TD>
      <TD CLASS = "thinright">
         30.64 ns
      </TD>
      <TD CLASS = "thinright">
         30.59 ns
      </TD>
      <TD>
         31.07 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         19.05 ns
      </TD>
      <TD CLASS = "thinright">
         18.69 ns
      </TD>
      <TD CLASS = "thinright">
         18.48 ns
      </TD>
      <TD>
         18.40 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         18.53 ns
      </TD>
      <TD CLASS = "thinright">
         19.01 ns
      </TD>
      <TD CLASS = "thinright">
         18.59 ns
      </TD>
      <TD>
         18.44 ns
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
         19.63 ns
      </TD>
      <TD CLASS = "thinright">
         31.05 ns
      </TD>
      <TD CLASS = "thinright">
         31.41 ns
      </TD>
      <TD>
         30.63 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         18.82 ns
      </TD>
      <TD CLASS = "thinright">
         18.39 ns
      </TD>
      <TD CLASS = "thinright">
         18.69 ns
      </TD>
      <TD>
         18.69 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         19.01 ns
      </TD>
      <TD CLASS = "thinright">
         18.69 ns
      </TD>
      <TD CLASS = "thinright">
         18.56 ns
      </TD>
      <TD>
         18.50 ns
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
         31.39 ns
      </TD>
      <TD CLASS = "thinright">
         30.51 ns
      </TD>
      <TD CLASS = "thinright">
         30.47 ns
      </TD>
      <TD>
         30.87 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         18.91 ns
      </TD>
      <TD CLASS = "thinright">
         18.69 ns
      </TD>
      <TD CLASS = "thinright">
         18.94 ns
      </TD>
      <TD>
         18.79 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         18.45 ns
      </TD>
      <TD CLASS = "thinright">
         18.56 ns
      </TD>
      <TD CLASS = "thinright">
         18.61 ns
      </TD>
      <TD>
         18.77 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:


 There was 12 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 1.66 times faster than Alga
 * Fgl was 1.66 times faster than Alga

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
         28.81 ns
      </TD>
      <TD CLASS = "thinright">
         16.40 μs
      </TD>
      <TD CLASS = "thinright">
         3.606 ms
      </TD>
      <TD>
         568.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         22.20 ns
      </TD>
      <TD CLASS = "thinright">
         103.3 ns
      </TD>
      <TD CLASS = "thinright">
         928.6 ns
      </TD>
      <TD>
         10.54 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         22.76 ns
      </TD>
      <TD CLASS = "thinright">
         94.22 ns
      </TD>
      <TD CLASS = "thinright">
         809.5 ns
      </TD>
      <TD>
         8.913 μs
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
         29.67 ns
      </TD>
      <TD CLASS = "thinright">
         4.675 μs
      </TD>
      <TD CLASS = "thinright">
         122.1 μs
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
         22.24 ns
      </TD>
      <TD CLASS = "thinright">
         102.2 ns
      </TD>
      <TD CLASS = "thinright">
         952.0 ns
      </TD>
      <TD>
         10.83 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         22.80 ns
      </TD>
      <TD CLASS = "thinright">
         86.34 ns
      </TD>
      <TD CLASS = "thinright">
         791.6 ns
      </TD>
      <TD>
         9.149 μs
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
         8.662 μs
      </TD>
      <TD CLASS = "thinright">
         255.8 μs
      </TD>
      <TD CLASS = "thinright">
         2.845 ms
      </TD>
      <TD>
         31.71 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         153.7 ns
      </TD>
      <TD CLASS = "thinright">
         790.4 ns
      </TD>
      <TD CLASS = "thinright">
         3.562 μs
      </TD>
      <TD>
         17.27 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         120.4 ns
      </TD>
      <TD CLASS = "thinright">
         740.7 ns
      </TD>
      <TD CLASS = "thinright">
         3.349 μs
      </TD>
      <TD>
         13.93 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph was the fastest 7 times

 There was 5 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 22090.79 times faster than Alga
 * Fgl was 18657.82 times faster than Alga

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
         31.03 ns
      </TD>
      <TD CLASS = "thinright">
         611.4 ns
      </TD>
      <TD CLASS = "thinright">
         49.66 μs
      </TD>
      <TD>
         4.544 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         53.01 ns
      </TD>
      <TD CLASS = "thinright">
         674.5 ns
      </TD>
      <TD CLASS = "thinright">
         11.56 μs
      </TD>
      <TD>
         341.5 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.90 ns
      </TD>
      <TD CLASS = "thinright">
         34.12 ns
      </TD>
      <TD CLASS = "thinright">
         37.19 ns
      </TD>
      <TD>
         41.01 ns
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
         31.16 ns
      </TD>
      <TD CLASS = "thinright">
         285.3 ns
      </TD>
      <TD CLASS = "thinright">
         3.570 μs
      </TD>
      <TD>
         39.92 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         52.86 ns
      </TD>
      <TD CLASS = "thinright">
         236.5 ns
      </TD>
      <TD CLASS = "thinright">
         349.3 ns
      </TD>
      <TD>
         566.9 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.84 ns
      </TD>
      <TD CLASS = "thinright">
         32.95 ns
      </TD>
      <TD CLASS = "thinright">
         37.07 ns
      </TD>
      <TD>
         41.14 ns
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
         380.2 ns
      </TD>
      <TD CLASS = "thinright">
         5.295 μs
      </TD>
      <TD CLASS = "thinright">
         59.95 μs
      </TD>
      <TD>
         315.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         239.4 ns
      </TD>
      <TD CLASS = "thinright">
         985.9 ns
      </TD>
      <TD CLASS = "thinright">
         1.741 μs
      </TD>
      <TD>
         3.542 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         33.07 ns
      </TD>
      <TD CLASS = "thinright">
         36.65 ns
      </TD>
      <TD CLASS = "thinright">
         36.80 ns
      </TD>
      <TD>
         40.92 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph was the fastest 37 times

 There was 7 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 49670.26 times faster than Alga
 * Fgl was 36415.71 times faster than Alga

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
         46.95 ns
      </TD>
      <TD CLASS = "thinright">
         1.075 μs
      </TD>
      <TD CLASS = "thinright">
         109.0 μs
      </TD>
      <TD>
         15.53 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         121.8 ns
      </TD>
      <TD CLASS = "thinright">
         2.058 μs
      </TD>
      <TD CLASS = "thinright">
         193.4 μs
      </TD>
      <TD>
         28.93 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         168.0 ns
      </TD>
      <TD CLASS = "thinright">
         4.182 μs
      </TD>
      <TD CLASS = "thinright">
         367.9 μs
      </TD>
      <TD>
         39.65 ms
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
         48.52 ns
      </TD>
      <TD CLASS = "thinright">
         378.0 ns
      </TD>
      <TD CLASS = "thinright">
         4.537 μs
      </TD>
      <TD>
         42.54 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         122.5 ns
      </TD>
      <TD CLASS = "thinright">
         990.4 ns
      </TD>
      <TD CLASS = "thinright">
         9.444 μs
      </TD>
      <TD>
         93.67 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         170.0 ns
      </TD>
      <TD CLASS = "thinright">
         1.837 μs
      </TD>
      <TD CLASS = "thinright">
         20.10 μs
      </TD>
      <TD>
         240.4 μs
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
         577.0 ns
      </TD>
      <TD CLASS = "thinright">
         8.854 μs
      </TD>
      <TD CLASS = "thinright">
         70.72 μs
      </TD>
      <TD>
         606.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.427 μs
      </TD>
      <TD CLASS = "thinright">
         19.23 μs
      </TD>
      <TD CLASS = "thinright">
         140.4 μs
      </TD>
      <TD>
         1.276 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.720 μs
      </TD>
      <TD CLASS = "thinright">
         36.76 μs
      </TD>
      <TD CLASS = "thinright">
         296.4 μs
      </TD>
      <TD>
         2.690 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 42 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 3.79 times faster than Hash-Graph
 * Fgl was 1.86 times faster than Hash-Graph

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
         37.60 ns
      </TD>
      <TD CLASS = "thinright">
         1.117 μs
      </TD>
      <TD CLASS = "thinright">
         108.6 μs
      </TD>
      <TD>
         16.58 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         72.50 ns
      </TD>
      <TD CLASS = "thinright">
         1.847 μs
      </TD>
      <TD CLASS = "thinright">
         186.6 μs
      </TD>
      <TD>
         28.83 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         104.8 ns
      </TD>
      <TD CLASS = "thinright">
         4.073 μs
      </TD>
      <TD CLASS = "thinright">
         339.5 μs
      </TD>
      <TD>
         39.46 ms
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
         37.31 ns
      </TD>
      <TD CLASS = "thinright">
         362.5 ns
      </TD>
      <TD CLASS = "thinright">
         4.020 μs
      </TD>
      <TD>
         41.96 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         73.16 ns
      </TD>
      <TD CLASS = "thinright">
         801.2 ns
      </TD>
      <TD CLASS = "thinright">
         8.965 μs
      </TD>
      <TD>
         93.78 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         104.7 ns
      </TD>
      <TD CLASS = "thinright">
         1.552 μs
      </TD>
      <TD CLASS = "thinright">
         19.45 μs
      </TD>
      <TD>
         229.2 μs
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
         549.4 ns
      </TD>
      <TD CLASS = "thinright">
         10.60 μs
      </TD>
      <TD CLASS = "thinright">
         70.11 μs
      </TD>
      <TD>
         698.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.256 μs
      </TD>
      <TD CLASS = "thinright">
         18.65 μs
      </TD>
      <TD CLASS = "thinright">
         141.7 μs
      </TD>
      <TD>
         1.269 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.417 μs
      </TD>
      <TD CLASS = "thinright">
         35.31 μs
      </TD>
      <TD CLASS = "thinright">
         293.2 μs
      </TD>
      <TD>
         2.701 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 24 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 3.48 times faster than Hash-Graph
 * Fgl was 1.82 times faster than Hash-Graph

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
         33.53 ns
      </TD>
      <TD CLASS = "thinright">
         3.193 μs
      </TD>
      <TD CLASS = "thinright">
         436.0 μs
      </TD>
      <TD>
         157.6 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         62.66 ns
      </TD>
      <TD CLASS = "thinright">
         2.294 μs
      </TD>
      <TD CLASS = "thinright">
         195.7 μs
      </TD>
      <TD>
         28.71 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         71.09 ns
      </TD>
      <TD CLASS = "thinright">
         8.007 μs
      </TD>
      <TD CLASS = "thinright">
         605.2 μs
      </TD>
      <TD>
         57.93 ms
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
         33.02 ns
      </TD>
      <TD CLASS = "thinright">
         966.4 ns
      </TD>
      <TD CLASS = "thinright">
         14.14 μs
      </TD>
      <TD>
         167.4 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         66.09 ns
      </TD>
      <TD CLASS = "thinright">
         908.5 ns
      </TD>
      <TD CLASS = "thinright">
         9.363 μs
      </TD>
      <TD>
         93.31 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         70.98 ns
      </TD>
      <TD CLASS = "thinright">
         2.718 μs
      </TD>
      <TD CLASS = "thinright">
         28.32 μs
      </TD>
      <TD>
         316.3 μs
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
         1.510 μs
      </TD>
      <TD CLASS = "thinright">
         31.23 μs
      </TD>
      <TD CLASS = "thinright">
         279.1 μs
      </TD>
      <TD>
         3.279 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.346 μs
      </TD>
      <TD CLASS = "thinright">
         20.35 μs
      </TD>
      <TD CLASS = "thinright">
         142.6 μs
      </TD>
      <TD>
         1.281 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         3.988 μs
      </TD>
      <TD CLASS = "thinright">
         58.10 μs
      </TD>
      <TD CLASS = "thinright">
         423.1 μs
      </TD>
      <TD>
         3.384 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 18 times
 * Alga was the fastest 4 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Fgl was 3.28 times faster than Alga
 * Hash-Graph was 1.44 times faster than Alga

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
         17.85 μs
      </TD>
      <TD CLASS = "thinright">
         7.458 ms
      </TD>
      <TD>
         1.165 s
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         4.396 μs
      </TD>
      <TD CLASS = "thinright">
         223.5 μs
      </TD>
      <TD>
         31.67 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         4.188 μs
      </TD>
      <TD CLASS = "thinright">
         395.0 μs
      </TD>
      <TD>
         39.31 ms
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
         5.339 μs
      </TD>
      <TD CLASS = "thinright">
         74.00 μs
      </TD>
      <TD>
         2.022 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.511 μs
      </TD>
      <TD CLASS = "thinright">
         10.14 μs
      </TD>
      <TD>
         94.80 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1.531 μs
      </TD>
      <TD CLASS = "thinright">
         19.98 μs
      </TD>
      <TD>
         230.6 μs
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
         8.463 μs
      </TD>
      <TD CLASS = "thinright">
         172.9 μs
      </TD>
      <TD CLASS = "thinright">
         4.821 ms
      </TD>
      <TD>
         50.37 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1.970 μs
      </TD>
      <TD CLASS = "thinright">
         23.70 μs
      </TD>
      <TD CLASS = "thinright">
         155.1 μs
      </TD>
      <TD>
         1.342 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.438 μs
      </TD>
      <TD CLASS = "thinright">
         39.85 μs
      </TD>
      <TD CLASS = "thinright">
         304.7 μs
      </TD>
      <TD>
         2.689 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 25 times

 There was 5 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Fgl was 31.87 times faster than Alga
 * Hash-Graph was 19.08 times faster than Alga

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
         109.6 ns
      </TD>
      <TD CLASS = "thinright">
         1.889 μs
      </TD>
      <TD CLASS = "thinright">
         184.1 μs
      </TD>
      <TD>
         28.96 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         115.1 ns
      </TD>
      <TD CLASS = "thinright">
         3.655 μs
      </TD>
      <TD CLASS = "thinright">
         341.3 μs
      </TD>
      <TD>
         40.01 ms
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
         108.9 ns
      </TD>
      <TD CLASS = "thinright">
         895.8 ns
      </TD>
      <TD CLASS = "thinright">
         9.254 μs
      </TD>
      <TD>
         92.39 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         115.4 ns
      </TD>
      <TD CLASS = "thinright">
         1.527 μs
      </TD>
      <TD CLASS = "thinright">
         19.73 μs
      </TD>
      <TD>
         228.6 μs
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
         1.345 μs
      </TD>
      <TD CLASS = "thinright">
         19.05 μs
      </TD>
      <TD CLASS = "thinright">
         140.1 μs
      </TD>
      <TD>
         1.248 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         2.316 μs
      </TD>
      <TD CLASS = "thinright">
         36.87 μs
      </TD>
      <TD CLASS = "thinright">
         302.6 μs
      </TD>
      <TD>
         2.643 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl was the fastest 33 times
 * Hash-Graph was the fastest 2 times

 There was 1 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Fgl was 1.87 times faster than Hash-Graph

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
         17.34 ns
      </TD>
      <TD CLASS = "thinright">
         3.518 μs
      </TD>
      <TD CLASS = "thinright">
         406.1 μs
      </TD>
      <TD>
         40.31 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         49.89 ns
      </TD>
      <TD CLASS = "thinright">
         1.214 μs
      </TD>
      <TD CLASS = "thinright">
         150.1 μs
      </TD>
      <TD>
         78.92 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         38.99 ns
      </TD>
      <TD CLASS = "thinright">
         18.55 μs
      </TD>
      <TD CLASS = "thinright">
         13.79 ms
      </TD>
      <TD>
         3.465 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         28.98 ns
      </TD>
      <TD CLASS = "thinright">
         24.35 μs
      </TD>
      <TD CLASS = "thinright">
         6.629 ms
      </TD>
      <TD>
         3.431 s
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
         17.57 ns
      </TD>
      <TD CLASS = "thinright">
         1.056 μs
      </TD>
      <TD CLASS = "thinright">
         13.72 μs
      </TD>
      <TD>
         158.1 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         49.98 ns
      </TD>
      <TD CLASS = "thinright">
         528.9 ns
      </TD>
      <TD CLASS = "thinright">
         5.645 μs
      </TD>
      <TD>
         76.42 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         42.25 ns
      </TD>
      <TD CLASS = "thinright">
         6.187 μs
      </TD>
      <TD CLASS = "thinright">
         211.8 μs
      </TD>
      <TD>
         18.53 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         29.66 ns
      </TD>
      <TD CLASS = "thinright">
         6.947 μs
      </TD>
      <TD CLASS = "thinright">
         218.9 μs
      </TD>
      <TD>
         18.79 ms
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
         1.618 μs
      </TD>
      <TD CLASS = "thinright">
         31.57 μs
      </TD>
      <TD CLASS = "thinright">
         265.1 μs
      </TD>
      <TD>
         2.118 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         870.8 ns
      </TD>
      <TD CLASS = "thinright">
         14.93 μs
      </TD>
      <TD CLASS = "thinright">
         115.4 μs
      </TD>
      <TD>
         1.367 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         10.65 μs
      </TD>
      <TD CLASS = "thinright">
         539.6 μs
      </TD>
      <TD CLASS = "thinright">
         18.49 ms
      </TD>
      <TD>
         556.6 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         13.51 μs
      </TD>
      <TD CLASS = "thinright">
         565.9 μs
      </TD>
      <TD CLASS = "thinright">
         18.11 ms
      </TD>
      <TD>
         555.6 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 9 times
 * Alga was the fastest 3 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 105.15 times faster than Fgl
 * Containers was 102.19 times faster than Fgl
 * Hash-Graph was 1.01 times faster than Fgl

