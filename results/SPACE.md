The content of this file was obtained with:
```Bash
$ space run -d Html
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

Using [("Mesh",3),("Clique",3)] as graphs

Note: results are in bytes
## edgeList

Description: Produce a list of the edges in the graph

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
         184
      </TD>
      <TD CLASS = "thinright">
         17,432
      </TD>
      <TD>
         333,184
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
         4,024
      </TD>
      <TD>
         53,992
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
         2,232
      </TD>
      <TD>
         30,376
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
         184
      </TD>
      <TD CLASS = "thinright">
         61,976
      </TD>
      <TD>
         10,505,080
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
         13,200
      </TD>
      <TD>
         1,428,000
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
         7,144
      </TD>
      <TD>
         775,168
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 4 times
 * Hash-Graph used the least amount of memory 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 18.92 times lighter than Alga
 * Hash-Graph was 11.91 times lighter than Alga
 * Fgl was 6.60 times lighter than Alga

## vertexList

Description: Produce a list of the vertices in the graph

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
         112
      </TD>
      <TD CLASS = "thinright">
         6,416
      </TD>
      <TD>
         175,496
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
         816
      </TD>
      <TD>
         8,016
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
         1,800
      </TD>
      <TD>
         18,360
      </TD>
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
         8,240
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
         112
      </TD>
      <TD CLASS = "thinright">
         20,856
      </TD>
      <TD>
         5,013,368
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
         816
      </TD>
      <TD>
         8,016
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
         1,800
      </TD>
      <TD>
         18,360
      </TD>
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
         8,240
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph used the least amount of memory 4 times

 There was 2 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 42.31 times lighter than Alga
 * Hash-Graph was 41.16 times lighter than Alga
 * Fgl was 18.47 times lighter than Alga

## equality

Description: Test if two graphs are equals

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
         724
      </TD>
      <TD CLASS = "thinright">
         26,736
      </TD>
      <TD>
         501,908
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         140
      </TD>
      <TD CLASS = "thinright">
         332
      </TD>
      <TD>
         2,684
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1,324
      </TD>
      <TD CLASS = "thinright">
         21,248
      </TD>
      <TD>
         284,428
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         772
      </TD>
      <TD CLASS = "thinright">
         11,180
      </TD>
      <TD>
         167,520
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
         724
      </TD>
      <TD CLASS = "thinright">
         94,960
      </TD>
      <TD>
         15,969,676
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         140
      </TD>
      <TD CLASS = "thinright">
         716
      </TD>
      <TD>
         59,936
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1,324
      </TD>
      <TD CLASS = "thinright">
         60,728
      </TD>
      <TD>
         6,958,848
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         772
      </TD>
      <TD CLASS = "thinright">
         33,268
      </TD>
      <TD>
         4,923,880
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 10 times
 * Hash-Graph used the least amount of memory 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 295.97 times lighter than Alga
 * Hash-Graph was 16.41 times lighter than Alga
 * Fgl was 2.84 times lighter than Alga

## transpose

Description: Transpose (invert all the edges) the graph

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
         392
      </TD>
      <TD CLASS = "thinright">
         4,336
      </TD>
      <TD>
         50,112
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
         392
      </TD>
      <TD CLASS = "thinright">
         9,968
      </TD>
      <TD>
         889,808
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga used the least amount of memory 6 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 1.52 times lighter than Containers

## dff

Description: Produce a forest, obtainened from a DFS (Deep First Search) of each vertex

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
         424
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
         Fgl
      </TH>
      <TD CLASS = "thinright">
         784
      </TD>
      <TD CLASS = "thinright">
         12,960
      </TD>
      <TD>
         175,752
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         272
      </TD>
      <TD CLASS = "thinright">
         5,856
      </TD>
      <TD>
         114,592
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
         17,784
      </TD>
      <TD>
         1,231,296
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         272
      </TD>
      <TD CLASS = "thinright">
         8,752
      </TD>
      <TD>
         482,480
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph used the least amount of memory 4 times
 * Containers used the least amount of memory 1 times

 There was 1 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 2.20 times lighter than Fgl
 * Containers was 1.87 times lighter than Fgl

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
         18,504
      </TD>
      <TD>
         307,824
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
         28,120
      </TD>
      <TD>
         1,845,616
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 6 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 2.82 times lighter than Fgl

## reachable

Description: Produce a list of reachable vertices from a given one

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
         2,440
      </TD>
      <TD>
         25,384
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1,000
      </TD>
      <TD CLASS = "thinright">
         8,732
      </TD>
      <TD>
         104,444
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
         5,128
      </TD>
      <TD>
         426,148
      </TD>
   </TR>
   <TR>
      <TH>
         Fgl
      </TH>
      <TD CLASS = "thinright">
         1,000
      </TD>
      <TD CLASS = "thinright">
         13,256
      </TD>
      <TD>
         665,276
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 10 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 2.35 times lighter than Fgl

## vertexCount

Description: Count the vertices of the graph

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
         72
      </TD>
      <TD CLASS = "thinright">
         5,872
      </TD>
      <TD>
         169,912
      </TD>
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
         72
      </TD>
      <TD CLASS = "thinright">
         20,312
      </TD>
      <TD>
         5,007,784
      </TD>
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
</TABLE>


SUMMARY:


 There was 6 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Fgl was 20542.02 times lighter than Alga
 * Containers was 20542.02 times lighter than Alga
 * Hash-Graph was 10271.01 times lighter than Alga

## edgeCount

Description: Count the edges of the graph

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
         200
      </TD>
      <TD CLASS = "thinright">
         17,448
      </TD>
      <TD>
         333,200
      </TD>
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
         2,584
      </TD>
      <TD>
         33,960
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
         1,856
      </TD>
      <TD>
         20,592
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
         200
      </TD>
      <TD CLASS = "thinright">
         61,992
      </TD>
      <TD>
         10,505,096
      </TD>
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
         8,176
      </TD>
      <TD>
         873,616
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
         1,856
      </TD>
      <TD>
         20,592
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 6 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 16147.82 times lighter than Alga
 * Hash-Graph was 31.37 times lighter than Alga
 * Fgl was 9.88 times lighter than Alga

## hasEdge

Description: Test if the given edge is in the graph (with arguments both in the graph and not in the graph (where applicable))

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
         1,592
      </TD>
      <TD CLASS = "thinright">
         3,359
      </TD>
      <TD>
         3,596
      </TD>
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
         Fgl
      </TH>
      <TD CLASS = "thinright">
         488
      </TD>
      <TD CLASS = "thinright">
         1,559
      </TD>
      <TD>
         1,836
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
         1,592
      </TD>
      <TD CLASS = "thinright">
         6,212
      </TD>
      <TD>
         140,495
      </TD>
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
         Fgl
      </TH>
      <TD CLASS = "thinright">
         488
      </TD>
      <TD CLASS = "thinright">
         4,135
      </TD>
      <TD>
         46,728
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
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 26 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was Infinity times lighter than Alga
 * Hash-Graph was 25.86 times lighter than Alga
 * Fgl was 1.89 times lighter than Alga

## isEmpty

Description: Test if the graph is empty

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
         0
      </TD>
      <TD>
         0
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
         0
      </TD>
      <TD>
         0
      </TD>
   </TR>
</TABLE>


SUMMARY:


 There was 6 ex-aequo

## hasVertex

Description: Test if the given vertex is in the graph

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
         220
      </TD>
      <TD CLASS = "thinright">
         804
      </TD>
      <TD>
         1,136
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
         12
      </TD>
      <TD>
         12
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
         220
      </TD>
      <TD CLASS = "thinright">
         1,884
      </TD>
      <TD>
         25,654
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
         12
      </TD>
      <TD>
         12
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph used the least amount of memory 14 times

 There was 6 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 2.00 times lighter than Alga

## addEdge

Description: Add an edge (not already in the graph)

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
         Fgl
      </TH>
      <TD CLASS = "thinright">
         480
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
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         448
      </TD>
      <TD CLASS = "thinright">
         1,308
      </TD>
      <TD>
         5,938
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
         Fgl
      </TH>
      <TD CLASS = "thinright">
         480
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
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         448
      </TD>
      <TD CLASS = "thinright">
         1,308
      </TD>
      <TD>
         6,092
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga used the least amount of memory 18 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 75.17 times lighter than Hash-Graph
 * Fgl was 4.93 times lighter than Hash-Graph

## addVertex

Description: Add a vertex (not already in the graph)

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
         120
      </TD>
      <TD CLASS = "thinright">
         140
      </TD>
      <TD>
         200
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
         768
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
         Alga
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
         120
      </TD>
      <TD CLASS = "thinright">
         140
      </TD>
      <TD>
         200
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
         768
      </TD>
      <TD>
         5,184
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Alga used the least amount of memory 12 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Alga was 129.60 times lighter than Hash-Graph
 * Fgl was 25.92 times lighter than Hash-Graph

## removeVertex

Description: Remove a vertex of the graph

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
         32
      </TD>
      <TD CLASS = "thinright">
         572
      </TD>
      <TD>
         8,528
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
         1,020
      </TD>
      <TD>
         1,524
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         96
      </TD>
      <TD CLASS = "thinright">
         2,092
      </TD>
      <TD>
         7,324
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
         32
      </TD>
      <TD CLASS = "thinright">
         1,952
      </TD>
      <TD>
         333,536
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
         2,252
      </TD>
      <TD>
         29,652
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         96
      </TD>
      <TD CLASS = "thinright">
         6,568
      </TD>
      <TD>
         99,644
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl used the least amount of memory 6 times
 * Alga used the least amount of memory 5 times

 There was 1 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Fgl was 7.47 times lighter than Alga
 * Hash-Graph was 1.87 times lighter than Alga

## removeEdge

Description: Remove an edge of the graph

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
         Alga
      </TH>
      <TD CLASS = "thinright">
         10,691
      </TD>
      <TD>
         139,576
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
         3,397
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1,008
      </TD>
      <TD>
         5,627
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
         Alga
      </TH>
      <TD CLASS = "thinright">
         36,840
      </TD>
      <TD>
         3,960,144
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
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         1,157
      </TD>
      <TD>
         5,957
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Hash-Graph used the least amount of memory 9 times
 * Fgl used the least amount of memory 3 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Hash-Graph was 356.85 times lighter than Alga
 * Fgl was 39.52 times lighter than Alga

## mergeContext

Description: Merge a FGL context in the graph

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
         312
      </TD>
      <TD CLASS = "thinright">
         616
      </TD>
      <TD>
         968
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         325
      </TD>
      <TD CLASS = "thinright">
         843
      </TD>
      <TD>
         5,261
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
         312
      </TD>
      <TD CLASS = "thinright">
         659
      </TD>
      <TD>
         1,080
      </TD>
   </TR>
   <TR>
      <TH>
         Hash-Graph
      </TH>
      <TD CLASS = "thinright">
         325
      </TD>
      <TD CLASS = "thinright">
         843
      </TD>
      <TD>
         5,261
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Fgl used the least amount of memory 12 times
 * Hash-Graph used the least amount of memory 2 times

 There was 4 ex-aequo


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Fgl was 5.14 times lighter than Hash-Graph

## creation

Description: Create a graph from a list of edges

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
         5,360
      </TD>
      <TD>
         67,544
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         232
      </TD>
      <TD CLASS = "thinright">
         3,768
      </TD>
      <TD>
         39,456
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
         19,448
      </TD>
      <TD>
         303,560
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
         16,992
      </TD>
      <TD>
         273,192
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
         16,496
      </TD>
      <TD>
         1,751,696
      </TD>
   </TR>
   <TR>
      <TH>
         Containers
      </TH>
      <TD CLASS = "thinright">
         232
      </TD>
      <TD CLASS = "thinright">
         7,992
      </TD>
      <TD>
         693,072
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
         60,640
      </TD>
      <TD>
         8,294,120
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
         53,488
      </TD>
      <TD>
         8,445,400
      </TD>
   </TR>
</TABLE>


SUMMARY:

 * Containers used the least amount of memory 4 times
 * Alga used the least amount of memory 2 times


ABSTRACT:
(Based on an average of the ratio between largest benchmarks)

 * Containers was 9.37 times lighter than Fgl
 * Alga was 4.59 times lighter than Fgl
 * Hash-Graph was 1.06 times lighter than Fgl

