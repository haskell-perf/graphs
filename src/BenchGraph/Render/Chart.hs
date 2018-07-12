module BenchGraph.Render.Chart
  (
  mkChart
  )
where

import Control.Monad (void)
import Data.List (uncons, sort)
import Data.Maybe (fromJust)

import Graphics.Rendering.Chart.Easy hiding (uncons)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Grid

import Data.Map.Strict (Map, unionWith, fromList, elems)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import BenchGraph.Render.Types
import BenchGraph.Named
import BenchGraph.Render.Common

mkChart :: String
        -- ^ The name of the benchs
        -> (Double -> String)
        -- ^ A show function
        -> [Named (Grouped [Named Double])]
        -- ^ The data
        -> IO ()
mkChart _ _ [] = return ()
mkChart title s grouped = void $ renderableToFile svg "results.svg" $ fillBackground def $ gridToRenderable grid
  where
    grid = title' `wideAbove` (legend' `wideAbove` aboveN (map (besideN . map (layoutToGrid . (\x -> x {_layout_legend = Nothing}) . layout)) grp))
      where
        -- Group the benchs per line of 4 items
        grp = group 4 grouped

        -- Custom title
        title' = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre title
        ls = def { _font_size = 15 , _font_weight = FontWeightBold }

        -- Recreate the legend
        legend' = setPickFn nullPickFn $ toRenderable $ Legend legendStyle legendInfo
        hhgrp = head $ head grp
        legendStyle = fromJust $ _layout_legend $ layout hhgrp
        legendInfo = _plot_legend $ plotBars $ bars2 hhgrp

    -- Render to svg
    svg = def {_fo_format = SVG}

    layout e = layout_title .~ fst e
      $ layout_title_style . font_size .~ 10
      $ layout_y_axis . laxis_override .~ ((\x -> x {_axis_labels = [map (\(y,_) -> (y, s y)) $ head $ _axis_labels x]} ) . axisGridHide)
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars $ bars2 e ]
      $ def :: Layout PlotIndex Double

    bars2 e = plot_bars_titles .~ S.toList is
      $ plot_bars_values .~ addIndexes [value e]
      $ plot_bars_singleton_width .~ 100
      $ plot_bars_style .~ BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 30 5
      $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
      $ def
    value = elems . M.map average . mkValues is . getSimples . snd
    mkstyle c = (solidFillStyle c, Just (solidLine 1.0 $ opaque black))
    is = initSet grouped

mkValues :: Set String
         -- ^ This contains all the libs names. It allow to have coherent legends
         -> [[Named Double]] -> Map String [Double]
mkValues s = foldr (\vals -> unionWith (++) (fromList $ map (fmap return) vals)) (M.fromSet (const [0]) s)

group :: Int -> [a] -> [[a]]
group _ [] = [[]]
group i xs = f : group i l
  where
    (f,l) = splitAt i xs

initSet :: [Named (Grouped [Named Double])] -> Set String
initSet = foldr (\(_,vals) -> S.union (S.fromList $ tkLibsName vals)) S.empty

tkLibsName :: Grouped [Named Double] -> [String]
tkLibsName (Simple _ xs) = sort $ map fst xs
tkLibsName (Group xs) = maybe [] (tkLibsName . fst) $ uncons xs

