# Benchmarks

Doing:

----
* [addEdge](#addedge)
* [addVertex](#addvertex)
* [creation](#creation)
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

Using [("Clique",4)] as graphs

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
         44.80 ns
      </TD>
      <TD CLASS = "thinright">
         165.5 ns
      </TD>
      <TD CLASS = "thinright">
         1.203 μs
      </TD>
      <TD>
         11.28 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         76.18 ns
      </TD>
      <TD CLASS = "thinright">
         548.7 ns
      </TD>
      <TD CLASS = "thinright">
         38.71 μs
      </TD>
      <TD>
         23.49 ms
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
         2.156 μs
      </TD>
      <TD CLASS = "thinright">
         198.6 μs
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
         165.8 ns
      </TD>
      <TD CLASS = "thinright">
         3.959 μs
      </TD>
      <TD CLASS = "thinright">
         379.4 μs
      </TD>
      <TD>
         39.55 ms
      </TD>
   </TR>
</TABLE>


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 3506.42 times faster than Hash-Graph
 * Containers was 1.68 times faster than Hash-Graph
 * Fgl was 1.41 times faster than Hash-Graph

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
         34.91 ns
      </TD>
      <TD CLASS = "thinright">
         142.8 ns
      </TD>
      <TD CLASS = "thinright">
         1.057 μs
      </TD>
      <TD>
         10.72 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         94.81 ns
      </TD>
      <TD CLASS = "thinright">
         637.2 ns
      </TD>
      <TD CLASS = "thinright">
         39.74 μs
      </TD>
      <TD>
         24.49 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         75.32 ns
      </TD>
      <TD CLASS = "thinright">
         2.002 μs
      </TD>
      <TD CLASS = "thinright">
         195.7 μs
      </TD>
      <TD>
         27.97 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         98.69 ns
      </TD>
      <TD CLASS = "thinright">
         3.936 μs
      </TD>
      <TD CLASS = "thinright">
         375.1 μs
      </TD>
      <TD>
         41.25 ms
      </TD>
   </TR>
</TABLE>


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 3848.57 times faster than Hash-Graph
 * Containers was 1.68 times faster than Hash-Graph
 * Fgl was 1.47 times faster than Hash-Graph

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
         45.29 ns
      </TD>
      <TD CLASS = "thinright">
         627.5 ns
      </TD>
      <TD CLASS = "thinright">
         35.93 μs
      </TD>
      <TD>
         5.769 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         51.65 ns
      </TD>
      <TD CLASS = "thinright">
         1.212 μs
      </TD>
      <TD CLASS = "thinright">
         116.3 μs
      </TD>
      <TD>
         83.19 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         39.24 ns
      </TD>
      <TD CLASS = "thinright">
         18.53 μs
      </TD>
      <TD CLASS = "thinright">
         10.27 ms
      </TD>
      <TD>
         3.227 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         29.39 ns
      </TD>
      <TD CLASS = "thinright">
         21.98 μs
      </TD>
      <TD CLASS = "thinright">
         7.351 ms
      </TD>
      <TD>
         3.604 s
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 3 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 624.77 times faster than Hash-Graph
 * Containers was 43.33 times faster than Hash-Graph
 * Fgl was 1.12 times faster than Hash-Graph

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
         572.9 ns
      </TD>
      <TD CLASS = "thinright">
         14.35 μs
      </TD>
      <TD CLASS = "thinright">
         3.380 ms
      </TD>
      <TD>
         417.1 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         113.1 ns
      </TD>
      <TD CLASS = "thinright">
         2.335 μs
      </TD>
      <TD CLASS = "thinright">
         169.7 μs
      </TD>
      <TD>
         29.44 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         205.2 ns
      </TD>
      <TD CLASS = "thinright">
         5.940 μs
      </TD>
      <TD CLASS = "thinright">
         573.4 μs
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
         63.26 ns
      </TD>
      <TD CLASS = "thinright">
         5.782 μs
      </TD>
      <TD CLASS = "thinright">
         560.5 μs
      </TD>
      <TD>
         113.6 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 3 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 14.17 times faster than Alga
 * Hash-Graph was 3.67 times faster than Alga
 * Fgl was 3.45 times faster than Alga

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
         39.07 ns
      </TD>
      <TD CLASS = "thinright">
         3.561 μs
      </TD>
      <TD CLASS = "thinright">
         137.3 μs
      </TD>
      <TD>
         97.17 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         28.15 ns
      </TD>
      <TD CLASS = "thinright">
         257.2 ns
      </TD>
      <TD CLASS = "thinright">
         28.44 μs
      </TD>
      <TD>
         21.22 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         33.19 ns
      </TD>
      <TD CLASS = "thinright">
         1.876 μs
      </TD>
      <TD CLASS = "thinright">
         208.1 μs
      </TD>
      <TD>
         112.2 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         39.78 ns
      </TD>
      <TD CLASS = "thinright">
         1.004 μs
      </TD>
      <TD CLASS = "thinright">
         58.04 μs
      </TD>
      <TD>
         6.288 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 3 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 17.85 times faster than Fgl
 * Containers was 5.29 times faster than Fgl
 * Alga was 1.16 times faster than Fgl

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
         33.99 ns
      </TD>
      <TD CLASS = "thinright">
         3.597 μs
      </TD>
      <TD CLASS = "thinright">
         146.5 μs
      </TD>
      <TD>
         84.28 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         37.56 ns
      </TD>
      <TD CLASS = "thinright">
         861.9 ns
      </TD>
      <TD CLASS = "thinright">
         78.68 μs
      </TD>
      <TD>
         22.73 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         32.31 ns
      </TD>
      <TD CLASS = "thinright">
         2.974 μs
      </TD>
      <TD CLASS = "thinright">
         322.8 μs
      </TD>
      <TD>
         131.1 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         33.83 ns
      </TD>
      <TD CLASS = "thinright">
         1.792 μs
      </TD>
      <TD CLASS = "thinright">
         305.1 μs
      </TD>
      <TD>
         245.6 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 3 times

 There was 1 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 10.80 times faster than Hash-Graph
 * Alga was 2.91 times faster than Hash-Graph
 * Fgl was 1.87 times faster than Hash-Graph

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
         392.3 ns
      </TD>
      <TD CLASS = "thinright">
         4.616 μs
      </TD>
      <TD CLASS = "thinright">
         126.1 μs
      </TD>
      <TD>
         10.47 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         29.05 ns
      </TD>
      <TD CLASS = "thinright">
         266.4 ns
      </TD>
      <TD CLASS = "thinright">
         23.92 μs
      </TD>
      <TD>
         10.57 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         162.7 ns
      </TD>
      <TD CLASS = "thinright">
         13.87 μs
      </TD>
      <TD CLASS = "thinright">
         3.047 ms
      </TD>
      <TD>
         569.8 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         64.49 ns
      </TD>
      <TD CLASS = "thinright">
         4.106 μs
      </TD>
      <TD CLASS = "thinright">
         436.8 μs
      </TD>
      <TD>
         56.09 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 6 times
 * Alga was the fastest 1 times

 There was 1 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 104.41 times faster than Fgl
 * Alga was 77.93 times faster than Fgl
 * Hash-Graph was 18.54 times faster than Fgl

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
         37.33 ns
      </TD>
      <TD CLASS = "thinright">
         188.1 ns
      </TD>
      <TD CLASS = "thinright">
         1.282 μs
      </TD>
      <TD>
         13.74 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         27.25 ns
      </TD>
      <TD CLASS = "thinright">
         97.83 ns
      </TD>
      <TD CLASS = "thinright">
         898.0 ns
      </TD>
      <TD>
         9.442 μs
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
         1.351 μs
      </TD>
      <TD CLASS = "thinright">
         18.39 μs
      </TD>
      <TD>
         458.8 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         98.48 ns
      </TD>
      <TD CLASS = "thinright">
         148.0 ns
      </TD>
      <TD CLASS = "thinright">
         162.9 ns
      </TD>
      <TD>
         177.8 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph was the fastest 12 times
 * Containers was the fastest 6 times

 There was 1 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 3059.71 times faster than Fgl
 * Containers was 51.47 times faster than Fgl
 * Alga was 37.64 times faster than Fgl

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
         23.10 ns
      </TD>
      <TD CLASS = "thinright">
         100.0 ns
      </TD>
      <TD CLASS = "thinright">
         571.1 ns
      </TD>
      <TD>
         5.494 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         21.83 ns
      </TD>
      <TD CLASS = "thinright">
         22.00 ns
      </TD>
      <TD CLASS = "thinright">
         21.64 ns
      </TD>
      <TD>
         21.65 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         43.11 ns
      </TD>
      <TD CLASS = "thinright">
         613.1 ns
      </TD>
      <TD CLASS = "thinright">
         11.56 μs
      </TD>
      <TD>
         327.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.55 ns
      </TD>
      <TD CLASS = "thinright">
         32.01 ns
      </TD>
      <TD CLASS = "thinright">
         37.02 ns
      </TD>
      <TD>
         41.35 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 11 times

 There was 3 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 11.74 times faster than Alga
 * Hash-Graph was 6.26 times faster than Alga
 * Fgl was 1.42 times faster than Alga

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
         19.80 ns
      </TD>
      <TD CLASS = "thinright">
         23.74 ns
      </TD>
      <TD CLASS = "thinright">
         22.80 ns
      </TD>
      <TD>
         23.10 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         18.15 ns
      </TD>
      <TD CLASS = "thinright">
         18.08 ns
      </TD>
      <TD CLASS = "thinright">
         18.06 ns
      </TD>
      <TD>
         18.05 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         18.90 ns
      </TD>
      <TD CLASS = "thinright">
         19.26 ns
      </TD>
      <TD CLASS = "thinright">
         18.28 ns
      </TD>
      <TD>
         18.83 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         17.80 ns
      </TD>
      <TD CLASS = "thinright">
         19.05 ns
      </TD>
      <TD CLASS = "thinright">
         17.97 ns
      </TD>
      <TD>
         19.58 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:


 There was 4 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 1.28 times faster than Alga
 * Fgl was 1.23 times faster than Alga
 * Hash-Graph was 1.18 times faster than Alga

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
         110.9 ns
      </TD>
      <TD CLASS = "thinright">
         1.989 μs
      </TD>
      <TD CLASS = "thinright">
         198.2 μs
      </TD>
      <TD>
         28.23 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         117.5 ns
      </TD>
      <TD CLASS = "thinright">
         3.667 μs
      </TD>
      <TD CLASS = "thinright">
         379.8 μs
      </TD>
      <TD>
         39.42 ms
      </TD>
   </TR>
</TABLE>

Not implemented for Containers because it is a nonsense.
Not implemented for Alga because it is a nonsense.

SUMMARY:

 * Fgl was the fastest 11 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Fgl was 1.39 times faster than Hash-Graph

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
         743.8 ns
      </TD>
      <TD CLASS = "thinright">
         9.689 μs
      </TD>
      <TD CLASS = "thinright">
         1.586 ms
      </TD>
      <TD>
         213.2 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         122.6 ns
      </TD>
      <TD CLASS = "thinright">
         1.063 μs
      </TD>
      <TD CLASS = "thinright">
         81.28 μs
      </TD>
      <TD>
         16.26 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         279.4 ns
      </TD>
      <TD CLASS = "thinright">
         4.613 μs
      </TD>
      <TD CLASS = "thinright">
         320.0 μs
      </TD>
      <TD>
         61.30 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         61.33 ns
      </TD>
      <TD CLASS = "thinright">
         2.891 μs
      </TD>
      <TD CLASS = "thinright">
         279.1 μs
      </TD>
      <TD>
         68.27 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph was the fastest 4 times
 * Containers was the fastest 3 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 221.32 times faster than Alga
 * Containers was 25.66 times faster than Alga
 * Fgl was 3.46 times faster than Alga

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
         2.381 μs
      </TD>
      <TD CLASS = "thinright">
         20.60 μs
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
         644.8 ns
      </TD>
      <TD CLASS = "thinright">
         39.54 μs
      </TD>
      <TD>
         23.64 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         4.435 μs
      </TD>
      <TD CLASS = "thinright">
         238.1 μs
      </TD>
      <TD>
         30.54 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         4.026 μs
      </TD>
      <TD CLASS = "thinright">
         367.6 μs
      </TD>
      <TD>
         41.58 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 6 times
 * Containers was the fastest 3 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 163.06 times faster than Hash-Graph
 * Containers was 1.76 times faster than Hash-Graph
 * Fgl was 1.36 times faster than Hash-Graph

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
         30.90 ns
      </TD>
      <TD CLASS = "thinright">
         340.8 ns
      </TD>
      <TD CLASS = "thinright">
         2.928 μs
      </TD>
      <TD>
         30.81 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         34.90 ns
      </TD>
      <TD CLASS = "thinright">
         755.9 ns
      </TD>
      <TD CLASS = "thinright">
         42.36 μs
      </TD>
      <TD>
         22.37 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         83.19 ns
      </TD>
      <TD CLASS = "thinright">
         2.529 μs
      </TD>
      <TD CLASS = "thinright">
         213.2 μs
      </TD>
      <TD>
         29.73 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         78.62 ns
      </TD>
      <TD CLASS = "thinright">
         6.574 μs
      </TD>
      <TD CLASS = "thinright">
         435.7 μs
      </TD>
      <TD>
         42.43 ms
      </TD>
   </TR>
</TABLE>


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 1377.35 times faster than Hash-Graph
 * Containers was 1.90 times faster than Hash-Graph
 * Fgl was 1.43 times faster than Hash-Graph

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
         586.5 ns
      </TD>
      <TD CLASS = "thinright">
         15.48 μs
      </TD>
      <TD CLASS = "thinright">
         2.971 ms
      </TD>
      <TD>
         395.6 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         153.3 ns
      </TD>
      <TD CLASS = "thinright">
         2.491 μs
      </TD>
      <TD CLASS = "thinright">
         172.2 μs
      </TD>
      <TD>
         28.08 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         421.3 ns
      </TD>
      <TD CLASS = "thinright">
         9.675 μs
      </TD>
      <TD CLASS = "thinright">
         820.7 μs
      </TD>
      <TD>
         142.3 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         206.5 ns
      </TD>
      <TD CLASS = "thinright">
         7.264 μs
      </TD>
      <TD CLASS = "thinright">
         339.8 μs
      </TD>
      <TD>
         36.24 ms
      </TD>
   </TR>
</TABLE>


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 14.09 times faster than Alga
 * Hash-Graph was 10.92 times faster than Alga
 * Fgl was 2.78 times faster than Alga

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
         25.77 ns
      </TD>
      <TD CLASS = "thinright">
         313.6 ns
      </TD>
      <TD CLASS = "thinright">
         2.838 μs
      </TD>
      <TD>
         28.99 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         88.96 ns
      </TD>
      <TD CLASS = "thinright">
         2.061 μs
      </TD>
      <TD CLASS = "thinright">
         272.4 μs
      </TD>
      <TD>
         96.98 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         114.6 ns
      </TD>
      <TD CLASS = "thinright">
         12.88 μs
      </TD>
      <TD CLASS = "thinright">
         2.182 ms
      </TD>
      <TD>
         396.2 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         52.62 ns
      </TD>
      <TD CLASS = "thinright">
         13.91 μs
      </TD>
      <TD CLASS = "thinright">
         4.246 ms
      </TD>
      <TD>
         773.6 ms
      </TD>
   </TR>
</TABLE>


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 26686.27 times faster than Hash-Graph
 * Containers was 7.98 times faster than Hash-Graph
 * Fgl was 1.95 times faster than Hash-Graph

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
         30.68 ns
      </TD>
      <TD CLASS = "thinright">
         201.0 ns
      </TD>
      <TD CLASS = "thinright">
         2.281 μs
      </TD>
      <TD>
         37.06 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         20.87 ns
      </TD>
      <TD CLASS = "thinright">
         20.95 ns
      </TD>
      <TD CLASS = "thinright">
         20.94 ns
      </TD>
      <TD>
         20.90 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         20.56 ns
      </TD>
      <TD CLASS = "thinright">
         102.3 ns
      </TD>
      <TD CLASS = "thinright">
         917.4 ns
      </TD>
      <TD>
         10.53 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         22.32 ns
      </TD>
      <TD CLASS = "thinright">
         83.46 ns
      </TD>
      <TD CLASS = "thinright">
         792.2 ns
      </TD>
      <TD>
         8.835 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 3 times

 There was 1 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 1773.10 times faster than Alga
 * Hash-Graph was 4.19 times faster than Alga
 * Fgl was 3.52 times faster than Alga

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
         40.90 ns
      </TD>
      <TD CLASS = "thinright">
         308.6 ns
      </TD>
      <TD CLASS = "thinright">
         3.293 μs
      </TD>
      <TD>
         45.92 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         31.70 ns
      </TD>
      <TD CLASS = "thinright">
         150.3 ns
      </TD>
      <TD CLASS = "thinright">
         1.072 μs
      </TD>
      <TD>
         10.39 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         53.32 ns
      </TD>
      <TD CLASS = "thinright">
         459.8 ns
      </TD>
      <TD CLASS = "thinright">
         4.914 μs
      </TD>
      <TD>
         46.78 μs
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
         201.0 ns
      </TD>
      <TD CLASS = "thinright">
         2.247 μs
      </TD>
      <TD>
         26.21 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 3 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 4.50 times faster than Fgl
 * Hash-Graph was 1.79 times faster than Fgl
 * Alga was 1.02 times faster than Fgl

