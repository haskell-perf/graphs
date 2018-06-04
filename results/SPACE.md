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

Using [("Mesh",3),("Clique",3)] as graphs

Note: results are in bytes
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD>
         0
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD>
         0
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD CLASS = "thinright">
         80
      </TD>
      <TD>
         80
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD>
         0
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD>
         0
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD CLASS = "thinright">
         80
      </TD>
      <TD>
         80
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 6 ex-aequo

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24
      </TD>
      <TD CLASS = "thinright">
         736
      </TD>
      <TD>
         8,016
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         96
      </TD>
      <TD CLASS = "thinright">
         816
      </TD>
      <TD>
         8,240
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         96
      </TD>
      <TD CLASS = "thinright">
         1,800
      </TD>
      <TD>
         18,360
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         152
      </TD>
      <TD CLASS = "thinright">
         3,200
      </TD>
      <TD>
         87,400
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         24
      </TD>
      <TD CLASS = "thinright">
         736
      </TD>
      <TD>
         8,016
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         96
      </TD>
      <TD CLASS = "thinright">
         816
      </TD>
      <TD>
         8,240
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         96
      </TD>
      <TD CLASS = "thinright">
         1,800
      </TD>
      <TD>
         18,360
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         152
      </TD>
      <TD CLASS = "thinright">
         5,760
      </TD>
      <TD>
         1,815,096
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph used the least amount of memory 4 times
There was 2 ex-aequo


ABSTRACT:

 * Hash-Graph was 6.27 times lighter than Alga
 * Containers was 4.56 times lighter than Alga
 * Fgl was 2.12 times lighter than Alga

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD>
         16
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD>
         16
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         32
      </TD>
      <TD CLASS = "thinright">
         32
      </TD>
      <TD>
         32
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         120
      </TD>
      <TD CLASS = "thinright">
         3,296
      </TD>
      <TD>
         88,216
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD>
         16
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD>
         16
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         32
      </TD>
      <TD CLASS = "thinright">
         32
      </TD>
      <TD>
         32
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         120
      </TD>
      <TD CLASS = "thinright">
         5,856
      </TD>
      <TD>
         1,914,216
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 6 ex-aequo


ABSTRACT:

 * Fgl was 21.86 times lighter than Alga
 * Containers was 21.86 times lighter than Alga
 * Hash-Graph was 10.93 times lighter than Alga

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD>
         16
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         8
      </TD>
      <TD CLASS = "thinright">
         11
      </TD>
      <TD>
         11
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         220
      </TD>
      <TD CLASS = "thinright">
         720
      </TD>
      <TD>
         1,056
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD>
         16
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         8
      </TD>
      <TD CLASS = "thinright">
         11
      </TD>
      <TD>
         11
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         220
      </TD>
      <TD CLASS = "thinright">
         1,635
      </TD>
      <TD>
         22,635
      </TD>
   </TR>
</TABLE>


SUMMARY:

There was 16 ex-aequo


ABSTRACT:

 * Hash-Graph was Infinity times lighter than Alga
 * Fgl was Infinity times lighter than Alga

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         40
      </TD>
      <TD CLASS = "thinright">
         40
      </TD>
      <TD>
         40
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         48
      </TD>
      <TD CLASS = "thinright">
         1,856
      </TD>
      <TD>
         20,592
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         64
      </TD>
      <TD CLASS = "thinright">
         2,584
      </TD>
      <TD>
         33,960
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         312
      </TD>
      <TD CLASS = "thinright">
         20,984
      </TD>
      <TD>
         381,888
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         40
      </TD>
      <TD CLASS = "thinright">
         40
      </TD>
      <TD>
         40
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         48
      </TD>
      <TD CLASS = "thinright">
         1,856
      </TD>
      <TD>
         20,592
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         64
      </TD>
      <TD CLASS = "thinright">
         8,176
      </TD>
      <TD>
         873,616
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         312
      </TD>
      <TD CLASS = "thinright">
         74,232
      </TD>
      <TD>
         11,851,496
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 6 times


ABSTRACT:

 * Containers was 23.17 times lighter than Alga
 * Fgl was 6.52 times lighter than Alga
 * Hash-Graph was 5.01 times lighter than Alga

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         1,920
      </TD>
      <TD>
         22,400
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         32
      </TD>
      <TD CLASS = "thinright">
         2,232
      </TD>
      <TD>
         30,376
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         160
      </TD>
      <TD CLASS = "thinright">
         4,024
      </TD>
      <TD>
         53,992
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         296
      </TD>
      <TD CLASS = "thinright">
         20,968
      </TD>
      <TD>
         381,872
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         4,480
      </TD>
      <TD>
         404,080
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         32
      </TD>
      <TD CLASS = "thinright">
         7,144
      </TD>
      <TD>
         775,168
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         160
      </TD>
      <TD CLASS = "thinright">
         13,200
      </TD>
      <TD>
         1,428,000
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         296
      </TD>
      <TD CLASS = "thinright">
         74,216
      </TD>
      <TD>
         11,851,480
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 4 times
 * Hash-Graph used the least amount of memory 2 times


ABSTRACT:

 * Containers was 13.67 times lighter than Alga
 * Hash-Graph was 13.16 times lighter than Alga
 * Fgl was 7.02 times lighter than Alga

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD>
         0
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         208
      </TD>
      <TD CLASS = "thinright">
         252
      </TD>
      <TD>
         252
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         488
      </TD>
      <TD CLASS = "thinright">
         1,502
      </TD>
      <TD>
         1,782
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         2,112
      </TD>
      <TD CLASS = "thinright">
         17,244
      </TD>
      <TD>
         174,284
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD>
         0
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         208
      </TD>
      <TD CLASS = "thinright">
         252
      </TD>
      <TD>
         252
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         488
      </TD>
      <TD CLASS = "thinright">
         3,910
      </TD>
      <TD>
         47,298
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         2,112
      </TD>
      <TD CLASS = "thinright">
         55,566
      </TD>
      <TD>
         5,189,622
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 18 times


ABSTRACT:

 * Containers was Infinity times lighter than Alga
 * Hash-Graph was 64.46 times lighter than Alga
 * Fgl was 8.68 times lighter than Alga

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         80
      </TD>
      <TD CLASS = "thinright">
         80
      </TD>
      <TD>
         80
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         448
      </TD>
      <TD CLASS = "thinright">
         920
      </TD>
      <TD>
         1,150
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         480
      </TD>
      <TD CLASS = "thinright">
         1,386
      </TD>
      <TD>
         6,058
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         80
      </TD>
      <TD CLASS = "thinright">
         80
      </TD>
      <TD>
         80
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         448
      </TD>
      <TD CLASS = "thinright">
         940
      </TD>
      <TD>
         1,300
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         480
      </TD>
      <TD CLASS = "thinright">
         1,380
      </TD>
      <TD>
         6,236
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga used the least amount of memory 18 times


ABSTRACT:

 * Alga was 19.47 times lighter than Hash-Graph
 * Fgl was 1.67 times lighter than Hash-Graph

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         24
      </TD>
      <TD CLASS = "thinright">
         24
      </TD>
      <TD>
         24
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         96
      </TD>
      <TD CLASS = "thinright">
         136
      </TD>
      <TD>
         176
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         248
      </TD>
      <TD CLASS = "thinright">
         744
      </TD>
      <TD>
         5,232
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         24
      </TD>
      <TD CLASS = "thinright">
         24
      </TD>
      <TD>
         24
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         96
      </TD>
      <TD CLASS = "thinright">
         136
      </TD>
      <TD>
         176
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         248
      </TD>
      <TD CLASS = "thinright">
         744
      </TD>
      <TD>
         5,232
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga used the least amount of memory 6 times


ABSTRACT:

 * Alga was 22.45 times lighter than Hash-Graph
 * Fgl was 4.21 times lighter than Hash-Graph

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD CLASS = "thinright">
         856
      </TD>
      <TD>
         1,136
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         48
      </TD>
      <TD CLASS = "thinright">
         1,880
      </TD>
      <TD>
         6,752
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         48
      </TD>
      <TD CLASS = "thinright">
         2,616
      </TD>
      <TD>
         37,144
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD CLASS = "thinright">
         1,912
      </TD>
      <TD>
         23,672
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         48
      </TD>
      <TD CLASS = "thinright">
         6,912
      </TD>
      <TD>
         106,280
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         48
      </TD>
      <TD CLASS = "thinright">
         9,160
      </TD>
      <TD>
         1,027,960
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl used the least amount of memory 6 times


ABSTRACT:

 * Fgl was Infinity times lighter than Alga
 * Hash-Graph was 3.32 times lighter than Alga

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         164
      </TD>
      <TD CLASS = "thinright">
         356
      </TD>
      <TD>
         2,708
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         796
      </TD>
      <TD CLASS = "thinright">
         11,516
      </TD>
      <TD>
         176,136
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         1,056
      </TD>
      <TD CLASS = "thinright">
         21,464
      </TD>
      <TD>
         284,644
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1,360
      </TD>
      <TD CLASS = "thinright">
         32,696
      </TD>
      <TD>
         581,572
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         164
      </TD>
      <TD CLASS = "thinright">
         740
      </TD>
      <TD>
         59,960
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         796
      </TD>
      <TD CLASS = "thinright">
         35,044
      </TD>
      <TD>
         5,330,248
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         1,056
      </TD>
      <TD CLASS = "thinright">
         60,944
      </TD>
      <TD>
         6,959,064
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1,360
      </TD>
      <TD CLASS = "thinright">
         115,128
      </TD>
      <TD>
         18,167,664
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 10 times
 * Hash-Graph used the least amount of memory 2 times


ABSTRACT:

 * Containers was 14.05 times lighter than Alga
 * Hash-Graph was 4.52 times lighter than Alga
 * Fgl was 1.03 times lighter than Alga

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1,056
      </TD>
      <TD>
         3,397
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         2,523
      </TD>
      <TD>
         5,723
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         10,899
      </TD>
      <TD>
         142,443
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1,237
      </TD>
      <TD>
         6,109
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         7,789
      </TD>
      <TD>
         104,045
      </TD>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         37,629
      </TD>
      <TD>
         4,040,133
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph used the least amount of memory 9 times
 * Fgl used the least amount of memory 3 times


ABSTRACT:

 * Hash-Graph was 23.33 times lighter than Alga
 * Fgl was 14.19 times lighter than Alga

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD CLASS = "thinright">
         1,800
      </TD>
      <TD>
         25,704
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         416
      </TD>
      <TD CLASS = "thinright">
         4,360
      </TD>
      <TD>
         50,136
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         0
      </TD>
      <TD CLASS = "thinright">
         6,408
      </TD>
      <TD>
         712,728
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         416
      </TD>
      <TD CLASS = "thinright">
         9,992
      </TD>
      <TD>
         889,832
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga used the least amount of memory 6 times


ABSTRACT:

 * Alga was Infinity times lighter than Containers

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         272
      </TD>
      <TD CLASS = "thinright">
         5,704
      </TD>
      <TD>
         64,552
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         424
      </TD>
      <TD CLASS = "thinright">
         6,048
      </TD>
      <TD>
         118,576
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         784
      </TD>
      <TD CLASS = "thinright">
         12,984
      </TD>
      <TD>
         175,776
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         272
      </TD>
      <TD CLASS = "thinright">
         8,944
      </TD>
      <TD>
         486,464
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         424
      </TD>
      <TD CLASS = "thinright">
         11,080
      </TD>
      <TD>
         866,080
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         784
      </TD>
      <TD CLASS = "thinright">
         17,808
      </TD>
      <TD>
         1,231,320
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph used the least amount of memory 4 times
 * Containers used the least amount of memory 1 times
There was 1 ex-aequo


ABSTRACT:

 * Hash-Graph was 2.20 times lighter than Fgl
 * Containers was 2.03 times lighter than Fgl

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         504
      </TD>
      <TD CLASS = "thinright">
         6,504
      </TD>
      <TD>
         72,552
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1,184
      </TD>
      <TD CLASS = "thinright">
         18,528
      </TD>
      <TD>
         307,848
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         504
      </TD>
      <TD CLASS = "thinright">
         11,880
      </TD>
      <TD>
         874,080
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1,184
      </TD>
      <TD CLASS = "thinright">
         28,144
      </TD>
      <TD>
         1,845,640
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 6 times


ABSTRACT:

 * Containers was 2.57 times lighter than Fgl

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         448
      </TD>
      <TD CLASS = "thinright">
         4,360
      </TD>
      <TD>
         49,528
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1,024
      </TD>
      <TD CLASS = "thinright">
         15,832
      </TD>
      <TD>
         207,328
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         448
      </TD>
      <TD CLASS = "thinright">
         9,736
      </TD>
      <TD>
         851,056
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1,024
      </TD>
      <TD CLASS = "thinright">
         22,760
      </TD>
      <TD>
         1,291,520
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 6 times


ABSTRACT:

 * Containers was 2.42 times lighter than Fgl

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         96
      </TD>
      <TD CLASS = "thinright">
         640
      </TD>
      <TD>
         880
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         184
      </TD>
      <TD CLASS = "thinright">
         688
      </TD>
      <TD>
         5,184
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         96
      </TD>
      <TD CLASS = "thinright">
         688
      </TD>
      <TD>
         1,008
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         184
      </TD>
      <TD CLASS = "thinright">
         768
      </TD>
      <TD>
         5,184
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph used the least amount of memory 3 times
 * Fgl used the least amount of memory 2 times
There was 1 ex-aequo


ABSTRACT:

 * Fgl was 2.34 times lighter than Hash-Graph

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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         3,792
      </TD>
      <TD>
         39,480
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         6,304
      </TD>
      <TD>
         80,440
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         17,616
      </TD>
      <TD>
         290,376
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         256
      </TD>
      <TD CLASS = "thinright">
         19,448
      </TD>
      <TD>
         303,560
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
      <TH>
         100
      </TH>
   </TR>
   <TR>
      <TH>
         Alga
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         8,016
      </TD>
      <TD>
         693,096
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         19,744
      </TD>
      <TD>
         2,108,104
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         16
      </TD>
      <TD CLASS = "thinright">
         56,992
      </TD>
      <TD>
         8,294,120
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         256
      </TD>
      <TD CLASS = "thinright">
         60,640
      </TD>
      <TD>
         9,258,088
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 4 times
There was 2 ex-aequo


ABSTRACT:

 * Containers was 2.94 times lighter than Fgl
 * Alga was 1.89 times lighter than Fgl
 * Hash-Graph was 1.01 times lighter than Fgl

