{-# LANGUAGE TupleSections #-}

module BenchGraph.DataSize (
  computeSize
) where

import GHC.DataSize

import Control.DeepSeq (NFData, ($!!))
import Control.Monad (when)

import BenchGraph.GenericGraph
import BenchGraph.Utils (graphs, defaultGr)
import BenchGraph.Named

computeSize :: (NFData g) => [(String,Int)] -> (Edges -> g) -> IO [Named [Named Word]]
computeSize gr fun = mapM (\((gname, gfunc),ss) -> sequence $ (gname,) $ mapM (\s -> sequence $ (show s,) $ recursiveSize $!! fun $ gfunc s) ss) $ graphs gr
