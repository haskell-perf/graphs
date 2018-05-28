module BenchGraph.Mesh (
  mesh
) where

import BenchGraph.Named
import BenchGraph.Path
import BenchGraph.GenericGraph (Edges,GenericGraph(..))

import Control.Comonad (extract)

mesh :: GenericGraph
mesh = Named "Mesh" mkMesh

mkMesh :: Int -> Edges
mkMesh n = verticals ++ horizontals
  where
    verticals = concatMap (\n' -> map (\(x,y) -> (x+n',y+n')) firstV) [0..n]
    firstV = map (\(x,y) -> (x*(n+1),y*(n+1))) firstH
    horizontals = concatMap (\n' -> map (\(x,y) -> (x+(n+1)*n',y+(n+1)*n')) firstH ) [0..n]
    firstH = extract path n
