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
         22.89 ns
      </TD>
      <TD CLASS = "thinright">
         25.93 ns
      </TD>
      <TD CLASS = "thinright">
         25.97 ns
      </TD>
      <TD>
         26.06 ns
      </TD>
   </TR>
</TABLE>

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
         45.60 ns
      </TD>
      <TD CLASS = "thinright">
         984.1 ns
      </TD>
      <TD CLASS = "thinright">
         17.32 μs
      </TD>
      <TD>
         277.7 μs
      </TD>
   </TR>
</TABLE>

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
         77.00 ns
      </TD>
      <TD CLASS = "thinright">
         1.545 μs
      </TD>
      <TD CLASS = "thinright">
         27.22 μs
      </TD>
      <TD>
         434.6 μs
      </TD>
   </TR>
</TABLE>

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
         27.96 ns
      </TD>
      <TD CLASS = "thinright">
         91.01 ns
      </TD>
      <TD CLASS = "thinright">
         627.5 ns
      </TD>
      <TD>
         6.033 μs
      </TD>
   </TR>
</TABLE>

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
         63.25 ns
      </TD>
      <TD CLASS = "thinright">
         5.920 μs
      </TD>
      <TD CLASS = "thinright">
         337.7 μs
      </TD>
      <TD>
         81.44 ms
      </TD>
   </TR>
</TABLE>

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
         56.13 ns
      </TD>
      <TD CLASS = "thinright">
         6.013 μs
      </TD>
      <TD CLASS = "thinright">
         348.9 μs
      </TD>
      <TD>
         83.06 ms
      </TD>
   </TR>
</TABLE>

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
         679.6 ns
      </TD>
      <TD CLASS = "thinright">
         4.484 μs
      </TD>
      <TD CLASS = "thinright">
         30.04 μs
      </TD>
      <TD>
         405.1 μs
      </TD>
   </TR>
</TABLE>

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
         57.90 ns
      </TD>
      <TD CLASS = "thinright">
         179.8 ns
      </TD>
      <TD CLASS = "thinright">
         1.224 μs
      </TD>
      <TD>
         11.38 μs
      </TD>
   </TR>
</TABLE>

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
         36.65 ns
      </TD>
      <TD CLASS = "thinright">
         144.2 ns
      </TD>
      <TD CLASS = "thinright">
         1.117 μs
      </TD>
      <TD>
         11.20 μs
      </TD>
   </TR>
</TABLE>

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
         36.02 ns
      </TD>
      <TD CLASS = "thinright">
         390.6 ns
      </TD>
      <TD CLASS = "thinright">
         3.721 μs
      </TD>
      <TD>
         38.84 μs
      </TD>
   </TR>
</TABLE>

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
         493.4 ns
      </TD>
      <TD CLASS = "thinright">
         9.794 μs
      </TD>
      <TD CLASS = "thinright">
         513.4 μs
      </TD>
      <TD>
         120.3 ms
      </TD>
   </TR>
</TABLE>

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
         3.318 μs
      </TD>
      <TD CLASS = "thinright">
         29.35 μs
      </TD>
      <TD>
         354.2 μs
      </TD>
   </TR>
</TABLE>

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
         26.60 ns
      </TD>
      <TD CLASS = "thinright">
         293.9 ns
      </TD>
      <TD CLASS = "thinright">
         2.736 μs
      </TD>
      <TD>
         28.89 μs
      </TD>
   </TR>
</TABLE>

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
         33.08 ns
      </TD>
      <TD CLASS = "thinright">
         768.5 ns
      </TD>
      <TD CLASS = "thinright">
         39.25 μs
      </TD>
      <TD>
         7.559 ms
      </TD>
   </TR>
</TABLE>

