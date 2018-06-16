{-# LANGUAGE TupleSections #-}

module BenchGraph.DataSize (
  computeSize
) where

import GHC.DataSize

import Control.DeepSeq (NFData, ($!!))

import BenchGraph.GenericGraph
import BenchGraph.Utils (graphs)
import BenchGraph.Named

computeSize :: (NFData g) => [(String,Int)] -> (Edges -> g) -> IO [Named [Named Word]]
computeSize gr fun = mapM (\((gname, gfunc),ss) -> sequence $ (gname,) $ mapM (\s -> let (gr',sizeName) = gfunc s in sequence $ (show sizeName,) $ recursiveSize $!! fun gr') ss) $ graphs False gr
