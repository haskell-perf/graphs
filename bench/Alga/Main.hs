import Criterion.Main
import qualified Alga.Graph
import qualified Alga.NonEmptyGraph

main :: IO ()
main = defaultMain $ Alga.Graph.allBenchs ++ Alga.NonEmptyGraph.allBenchs  
