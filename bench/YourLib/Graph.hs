{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This file is an example about adding a library to the bench-suite
-- It will not compile without modification
--
-- First replace words starting by REPLACEWITH with the one that fits your library
-- Then go to bench/ListS.hs

module YourLib.Graph
  (functions, mk)
where

import BenchGraph.Types
import BenchGraph.GenericGraph (Edges)
import BenchGraph.Utils
import qualified BenchGraph.Suites as S

import REPLACEWITHYourSuperLibrary.YourGraphImplementation

instance GraphImpl REPLACEWITHYourGraphTypeWithIntVertices where
  mkGraph = mk
  mkVertex = REPLACEWITHyourFunctionThatCreateAGraphWithASingleVertex0

mk :: Edges -> REPLACEWITHYourGraphTypeWithIntVertices
mk e = REPLACEWITHyourFunctionThatCreateAGraphFromAListOfEdges

functions :: [Suite REPLACEWITHYourGraphTypeWithIntVertices]
functions =
  [
  -- Here go the functions that will be benchmarked
  -- You need to pass them to a Suite (full list in src/BenchGraph/Suites).
  -- For example if you want to bench your 'isEmpty'
    S.isEmpty REPLACEWITHyourFUnctionThatTestIfTheGraphIsEmpty
  -- Some Suite will need more arguments (generally how to convert a generic argument to on that fits your library's function. It is often 'id'
  -- For example:
  , S.hasVertex REPLACEWITHyourFunctionTahtTestIfAVertexIsInTheGraph REPLACEWITHaFunctionThatConvertAnIntToTheArgumentOfYourHasVertex
  -- others...
  --
  -- See examples of use in other libraries repository.
  ]

