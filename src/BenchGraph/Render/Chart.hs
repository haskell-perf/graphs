module BenchGraph.Render.Chart
  (
  mkChart
  )
where

import Data.List (uncons)

import Graphics.Rendering.Chart.Easy hiding (uncons)
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Map.Strict (Map,unionsWith, fromList, elems)
import qualified Data.Map.Strict as M

import BenchGraph.Render.Types
import BenchGraph.Named
import BenchGraph.Render.Common

mkChart :: String
        -- ^ The name of the bench
        -> Grouped [Named Double]
        -- ^ The data
        -> IO ()
mkChart name grouped = toFile def (name ++ ".png") $ do
    layout_title .= name
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot $ plotBars <$> bars (titles grouped) (addIndexes (map snd values))
  where
    titles (Simple _ xs) = map fst xs
    titles (Group xs) = maybe [] (titles . fst) $ uncons xs
    values = [(name, elems $ M.map average $ mkValues grouped)]

mkValues :: Grouped [Named Double] -> Map String [Double]
mkValues (Simple _ xs) = fromList  $ map (fmap return) xs
mkValues (Group xs) = unionsWith (++) $ map mkValues xs
