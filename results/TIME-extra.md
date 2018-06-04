# Note

Alga is representing graphs from an other point of view than Fgl, hash-graph and containers. Thus, creating graphs from a list of edges is not a simple thing to do with alga and can lead to very poor graphs representations.

Here are results using more "algebraic" structures as inputs for functions.

For information, these results were obtained by replacing the `mkGraph = edges` function by `mkGraph listOfEdges = clique [0..(extractMaxVertex listOfEdges)]`

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
* [creation](#creation)
----

Using [("Clique",4)] as graphs

## isEmpty

Descritpion: Test if the graph is empty

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
         22.69 ns
      </TD>
      <TD CLASS = "thinright">
         26.50 ns
      </TD>
      <TD CLASS = "thinright">
         25.82 ns
      </TD>
      <TD>
         25.70 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         21.20 ns
      </TD>
      <TD CLASS = "thinright">
         20.94 ns
      </TD>
      <TD CLASS = "thinright">
         22.52 ns
      </TD>
      <TD>
         21.59 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         20.85 ns
      </TD>
      <TD CLASS = "thinright">
         21.40 ns
      </TD>
      <TD CLASS = "thinright">
         20.48 ns
      </TD>
      <TD>
         19.50 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph was the fastest 1 times
There was 3 ex-aequo


ABSTRACT:

 * Hash-Graph was 1.23 times faster than Alga
 * Fgl was 1.17 times faster than Alga

## vertexList

Descritpion: Produce a list of the vertices in the graph

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
         46.17 ns
      </TD>
      <TD CLASS = "thinright">
         981.1 ns
      </TD>
      <TD CLASS = "thinright">
         17.04 μs
      </TD>
      <TD>
         275.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         31.88 ns
      </TD>
      <TD CLASS = "thinright">
         148.4 ns
      </TD>
      <TD CLASS = "thinright">
         1.062 μs
      </TD>
      <TD>
         10.37 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         58.39 ns
      </TD>
      <TD CLASS = "thinright">
         445.1 ns
      </TD>
      <TD CLASS = "thinright">
         4.438 μs
      </TD>
      <TD>
         43.86 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.77 ns
      </TD>
      <TD CLASS = "thinright">
         194.2 ns
      </TD>
      <TD CLASS = "thinright">
         2.245 μs
      </TD>
      <TD>
         24.55 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 3 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:

 * Containers was 12.66 times faster than Alga
 * Hash-Graph was 6.43 times faster than Alga
 * Fgl was 3.28 times faster than Alga

## vertexCount

Descritpion: Count the vertices of the graph

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
         74.20 ns
      </TD>
      <TD CLASS = "thinright">
         1.523 μs
      </TD>
      <TD CLASS = "thinright">
         26.75 μs
      </TD>
      <TD>
         434.7 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         21.21 ns
      </TD>
      <TD CLASS = "thinright">
         21.48 ns
      </TD>
      <TD CLASS = "thinright">
         20.26 ns
      </TD>
      <TD>
         20.45 ns
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         22.60 ns
      </TD>
      <TD CLASS = "thinright">
         102.5 ns
      </TD>
      <TD CLASS = "thinright">
         836.6 ns
      </TD>
      <TD>
         9.081 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24.28 ns
      </TD>
      <TD CLASS = "thinright">
         84.40 ns
      </TD>
      <TD CLASS = "thinright">
         725.8 ns
      </TD>
      <TD>
         7.556 μs
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 3 times
There was 1 ex-aequo


ABSTRACT:

 * Containers was 5663.64 times faster than Alga
 * Hash-Graph was 28.87 times faster than Alga
 * Fgl was 24.50 times faster than Alga

## hasVertex

Descritpion: Test if the given vertex is in the graph

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
         29.16 ns
      </TD>
      <TD CLASS = "thinright">
         93.55 ns
      </TD>
      <TD CLASS = "thinright">
         719.0 ns
      </TD>
      <TD>
         6.055 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         52.20 ns
      </TD>
      <TD CLASS = "thinright">
         590.0 ns
      </TD>
      <TD CLASS = "thinright">
         10.09 μs
      </TD>
      <TD>
         282.3 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         25.44 ns
      </TD>
      <TD CLASS = "thinright">
         33.28 ns
      </TD>
      <TD CLASS = "thinright">
         36.46 ns
      </TD>
      <TD>
         40.77 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph was the fastest 7 times
 * Alga was the fastest 3 times
There was 1 ex-aequo


ABSTRACT:

 * Hash-Graph was 53.17 times faster than Alga
 * Fgl was 30.04 times faster than Alga

## edgeCount

Descritpion: Count the edges of the graph

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
         64.61 ns
      </TD>
      <TD CLASS = "thinright">
         5.942 μs
      </TD>
      <TD CLASS = "thinright">
         337.7 μs
      </TD>
      <TD>
         57.75 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         28.73 ns
      </TD>
      <TD CLASS = "thinright">
         243.3 ns
      </TD>
      <TD CLASS = "thinright">
         25.59 μs
      </TD>
      <TD>
         21.52 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         50.49 ns
      </TD>
      <TD CLASS = "thinright">
         1.856 μs
      </TD>
      <TD CLASS = "thinright">
         198.9 μs
      </TD>
      <TD>
         82.45 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         39.07 ns
      </TD>
      <TD CLASS = "thinright">
         895.4 ns
      </TD>
      <TD CLASS = "thinright">
         52.79 μs
      </TD>
      <TD>
         5.646 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 3 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:

 * Containers was 10.64 times faster than Alga
 * Hash-Graph was 6.23 times faster than Alga
 * Fgl was 1.72 times faster than Alga

## edgeList

Descritpion: Produce a list of the edges in the graph

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
         55.53 ns
      </TD>
      <TD CLASS = "thinright">
         6.017 μs
      </TD>
      <TD CLASS = "thinright">
         349.1 μs
      </TD>
      <TD>
         58.13 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         36.55 ns
      </TD>
      <TD CLASS = "thinright">
         933.4 ns
      </TD>
      <TD CLASS = "thinright">
         79.62 μs
      </TD>
      <TD>
         26.53 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         48.78 ns
      </TD>
      <TD CLASS = "thinright">
         3.003 μs
      </TD>
      <TD CLASS = "thinright">
         321.9 μs
      </TD>
      <TD>
         96.27 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         35.25 ns
      </TD>
      <TD CLASS = "thinright">
         1.837 μs
      </TD>
      <TD CLASS = "thinright">
         303.3 μs
      </TD>
      <TD>
         176.3 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 3 times
There was 1 ex-aequo


ABSTRACT:

 * Containers was 3.64 times faster than Alga
 * Hash-Graph was 1.58 times faster than Alga
 * Fgl was 1.21 times faster than Alga

## hasEdge

Descritpion: Test if the given edge is in the graph (with arguments both in the graph and not in the graph (where applicable))

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
         703.8 ns
      </TD>
      <TD CLASS = "thinright">
         4.414 μs
      </TD>
      <TD CLASS = "thinright">
         28.63 μs
      </TD>
      <TD>
         398.0 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         26.90 ns
      </TD>
      <TD CLASS = "thinright">
         146.6 ns
      </TD>
      <TD CLASS = "thinright">
         1.248 μs
      </TD>
      <TD>
         14.64 μs
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
         1.274 μs
      </TD>
      <TD CLASS = "thinright">
         18.64 μs
      </TD>
      <TD>
         449.2 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         103.6 ns
      </TD>
      <TD CLASS = "thinright">
         154.3 ns
      </TD>
      <TD CLASS = "thinright">
         162.9 ns
      </TD>
      <TD>
         177.3 ns
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph was the fastest 10 times
 * Containers was the fastest 3 times


ABSTRACT:

 * Hash-Graph was 783.77 times faster than Alga
 * Containers was 26.80 times faster than Alga
 * Fgl was 2.43 times faster than Alga

## addEdge

Descritpion: Add an edge (not already in the graph)

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
         54.26 ns
      </TD>
      <TD CLASS = "thinright">
         180.8 ns
      </TD>
      <TD CLASS = "thinright">
         1.199 μs
      </TD>
      <TD>
         11.40 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         116.2 ns
      </TD>
      <TD CLASS = "thinright">
         2.100 μs
      </TD>
      <TD CLASS = "thinright">
         199.2 μs
      </TD>
      <TD>
         28.82 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         155.6 ns
      </TD>
      <TD CLASS = "thinright">
         3.924 μs
      </TD>
      <TD CLASS = "thinright">
         344.7 μs
      </TD>
      <TD>
         37.98 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 13 times


ABSTRACT:

 * Alga was 23.71 times faster than Hash-Graph
 * Fgl was 1.53 times faster than Hash-Graph

## addVertex

Descritpion: Add a vertex (not already in the graph)

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
         36.26 ns
      </TD>
      <TD CLASS = "thinright">
         151.6 ns
      </TD>
      <TD CLASS = "thinright">
         1.153 μs
      </TD>
      <TD>
         11.52 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         76.26 ns
      </TD>
      <TD CLASS = "thinright">
         2.029 μs
      </TD>
      <TD CLASS = "thinright">
         196.8 μs
      </TD>
      <TD>
         29.10 ms
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
         3.632 μs
      </TD>
      <TD CLASS = "thinright">
         332.9 μs
      </TD>
      <TD>
         37.78 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 4 times


ABSTRACT:

 * Alga was 10.74 times faster than Hash-Graph
 * Fgl was 1.49 times faster than Hash-Graph

## removeVertex

Descritpion: Remove a vertex of the graph

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
         37.71 ns
      </TD>
      <TD CLASS = "thinright">
         382.8 ns
      </TD>
      <TD CLASS = "thinright">
         3.707 μs
      </TD>
      <TD>
         36.53 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         41.08 ns
      </TD>
      <TD CLASS = "thinright">
         2.258 μs
      </TD>
      <TD CLASS = "thinright">
         206.6 μs
      </TD>
      <TD>
         29.63 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         50.15 ns
      </TD>
      <TD CLASS = "thinright">
         6.381 μs
      </TD>
      <TD CLASS = "thinright">
         401.9 μs
      </TD>
      <TD>
         43.18 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 3 times
There was 1 ex-aequo


ABSTRACT:

 * Alga was 4.87 times faster than Hash-Graph
 * Fgl was 1.35 times faster than Hash-Graph

## equality

Descritpion: Test if two graphs are equals

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
         483.3 ns
      </TD>
      <TD CLASS = "thinright">
         10.07 μs
      </TD>
      <TD CLASS = "thinright">
         518.6 μs
      </TD>
      <TD>
         93.03 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         30.77 ns
      </TD>
      <TD CLASS = "thinright">
         271.6 ns
      </TD>
      <TD CLASS = "thinright">
         24.85 μs
      </TD>
      <TD>
         11.08 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         167.5 ns
      </TD>
      <TD CLASS = "thinright">
         14.23 μs
      </TD>
      <TD CLASS = "thinright">
         2.152 ms
      </TD>
      <TD>
         340.4 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         68.00 ns
      </TD>
      <TD CLASS = "thinright">
         3.972 μs
      </TD>
      <TD CLASS = "thinright">
         460.3 μs
      </TD>
      <TD>
         58.99 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers was the fastest 7 times
There was 1 ex-aequo


ABSTRACT:

 * Containers was 217351.23 times faster than Alga
 * Hash-Graph was 33230.79 times faster than Alga
 * Fgl was 137.76 times faster than Alga

## removeEdge

Descritpion: Remove an edge of the graph

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
         3.313 μs
      </TD>
      <TD CLASS = "thinright">
         28.01 μs
      </TD>
      <TD>
         345.6 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         4.384 μs
      </TD>
      <TD CLASS = "thinright">
         231.6 μs
      </TD>
      <TD>
         30.67 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         3.803 μs
      </TD>
      <TD CLASS = "thinright">
         362.8 μs
      </TD>
      <TD>
         40.96 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 8 times
There was 1 ex-aequo


ABSTRACT:

 * Alga was 3.38 times faster than Fgl
 * Hash-Graph was 1.08 times faster than Fgl

## transpose

Descritpion: Transpose (invert all the edges) the graph

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
         287.2 ns
      </TD>
      <TD CLASS = "thinright">
         2.762 μs
      </TD>
      <TD>
         28.55 μs
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         89.73 ns
      </TD>
      <TD CLASS = "thinright">
         2.230 μs
      </TD>
      <TD CLASS = "thinright">
         270.8 μs
      </TD>
      <TD>
         82.75 ms
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 4 times


ABSTRACT:

 * Alga was 9.02 times faster than Containers

## creation

Descritpion: Create a graph from a list of edges

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
         31.71 ns
      </TD>
      <TD CLASS = "thinright">
         737.5 ns
      </TD>
      <TD CLASS = "thinright">
         37.65 μs
      </TD>
      <TD>
         5.816 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         54.51 ns
      </TD>
      <TD CLASS = "thinright">
         1.298 μs
      </TD>
      <TD CLASS = "thinright">
         122.4 μs
      </TD>
      <TD>
         59.35 ms
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         35.88 ns
      </TD>
      <TD CLASS = "thinright">
         18.37 μs
      </TD>
      <TD CLASS = "thinright">
         9.106 ms
      </TD>
      <TD>
         3.223 s
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         27.01 ns
      </TD>
      <TD CLASS = "thinright">
         22.18 μs
      </TD>
      <TD CLASS = "thinright">
         6.637 ms
      </TD>
      <TD>
         3.462 s
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga was the fastest 3 times
 * Hash-Graph was the fastest 1 times


ABSTRACT:

 * Alga was 4.30 times faster than Fgl
 * Containers was 1.67 times faster than Fgl
 * Hash-Graph was 1.31 times faster than Fgl


