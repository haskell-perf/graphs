module BenchGraph.Render.Chart
  (
  mkChart
  )
where

import Data.List (uncons, sort)

import Graphics.Rendering.Chart.Easy hiding (uncons)
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Map.Strict (Map, unionWith, fromList, elems)
import qualified Data.Map.Strict as M

import BenchGraph.Render.Types
import BenchGraph.Named
import BenchGraph.Render.Common

mkChart :: String
        -- ^ The name of the benchs
        -> [Named (Grouped [Named Double])]
        -- ^ The data
        -> IO ()
mkChart _ [] = return ()
mkChart title grouped = toFile def "results.png" $ do
    layout_title .= title
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst grouped)
    plot $ plotBars <$> bars (titles $ snd $ head grouped) (addIndexes values)
  where
    titles (Simple _ xs) = sort $ map fst xs
    titles (Group xs) = maybe [] (titles . fst) $ uncons xs
    values = map (elems . M.map average . mkValues . getSimples . snd) grouped

mkValues :: [[Named Double]] -> Map String [Double]
mkValues = foldr (\vals -> unionWith (++) (fromList $ map (fmap return) vals)) M.empty
