import BenchGraph (computeSize)
import BenchGraph.Named

import qualified Alga.Graph
import qualified Containers.Graph
import qualified Fgl.PatriciaTree
import qualified Fgl.Tree
import qualified HashGraph.Gr

printNArr ::  Named [Named [Named Word]] -> IO ()
printNArr (Named n c) = do
  putStrLn $ replicate 2 '#' ++ n
  mapM_ (\(Named n' c') -> do
      putStrLn $ replicate 3 '#' ++ n'
      mapM_ (\(Named n'' c'') -> putStrLn $ unwords [n'', ":", show c'']) c'
    ) c

main :: IO ()
main = do
  res <- mapM sequence
    [ Named "Alga (Algebra.Graph)" $ computeSize size Alga.Graph.mk
    , Named "Containers (Data.Graph)" $ computeSize size Containers.Graph.mk
    , Named "Fgl (Data.Graph.Inductive.PatriciaTree)" $ computeSize size Fgl.PatriciaTree.mk
    , Named "Fgl (Data.Graph.Inductive.Tree)" $ computeSize size Fgl.Tree.mk
    , Named "Hash-Graph (Data.HashGraph.Strict)" $ computeSize size HashGraph.Gr.mk
    ]
  mapM_ printNArr res
  where
    size = (3,3,2)
