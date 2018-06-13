module BenchGraph.RealLife.Graphs
  (
    realLife
  )
where

import BenchGraph.GenericGraph
import BenchGraph.RealLife.Generated (generated)

realLife :: GenericGraph
realLife = ("RealLife", grF)

grF :: Size -> (Edges,Int)
grF i = if i >= 0 && i < length generated
            then (generated !! i,i)
            else error $ "Does not have such graphs " ++ show i

