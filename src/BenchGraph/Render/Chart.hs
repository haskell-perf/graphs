module BenchGraph.Render.Chart
  (
  mkChart
  )
where

import Control.Monad (void)
import Data.List (uncons, sort)
import Data.Maybe (fromJust)

import Graphics.Rendering.Chart.Easy hiding (uncons, colors)
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
        -> ChartOutputFormat
        -- ^ The format
        -> [Named (Grouped [Named Double])]
        -- ^ The data
        -> IO ()
mkChart _ _ _ [] = return ()
mkChart title s chopt grouped =
  void $ renderableToFile fo ("results." ++ foExt) $ fillBackground def $ gridToRenderable grid
  where
    -- Group the benchs per line of 4 items
    grp = group 4 grouped

    grid = title' `wideAbove` (legend' `wideAbove` aboveN (map (besideN . map (layoutToGrid . (\x -> x {_layout_legend = Nothing}) . layout)) grp))
      where

        -- Custom title
        title' = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre title
        ls = def { _font_size = 15 , _font_weight = FontWeightBold }

        -- Recreate the legend
        legend' = setPickFn nullPickFn $ toRenderable $ Legend legendStyle legendInfo
        hhgrp = ("",Simple True $ zip (S.toList is) (replicate (S.size is) 0)) -- False data to generate the legend
        legendStyle = fromJust $ _layout_legend $ layout hhgrp
        legendInfo = _plot_legend $ plotBars $ bars2 hhgrp

    -- Render to svg
    (fo,foExt) = case chopt of
                   Png -> (def, "png")
                   Svg -> (def {_fo_format = SVG},"svg")

    layout e = layout_title .~ fst e
      $ layout_title_style . font_size .~ 10
      $ layout_y_axis . laxis_override .~ ((\x -> x {_axis_labels = [map (\(y,_) -> (y,s $ 10**(y - fromIntegral (fst (value e))))) $ head $ _axis_labels x]} ) . axisGridHide) -- Change the label with the corresponding 10 power
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars $ bars2 e ]
      $ def :: Layout PlotIndex Double

    bars2 e = plot_bars_titles .~ libsName
      $ plot_bars_values .~ addIndexes [snd $ value e]
      $ plot_bars_singleton_width .~ 100
      $ plot_bars_style .~ BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 30 5
      $ plot_bars_item_styles .~ map snd (filter (\(n,_) -> n `elem` libsName) colors)
      $ def
      where
        libsName= tkLibsName $ snd e

    value x = let maybeverysmall = filter (/=0) $ elems $ M.map average $ mkValues is $ getSimples $ snd x
                  tenp           = 1 + ceiling (abs $ minimum $ map (logBase 10) maybeverysmall) :: Int
               in (tenp, map (\u -> fromIntegral tenp + logBase 10 u) maybeverysmall) -- Make everyone > 1, so the log is positive.
    mkstyle c = (solidFillStyle c, Just (solidLine 1.0 $ opaque black))
    colors = let colo = take (S.size is) $ map mkstyle (cycle defaultColorSeq)
              in zip (S.toList is) colo
    is = initSet grouped

mkValues :: Set String
         -- ^ This contains all the libs names. It allow to have coherent legends
         -> [[Named Double]] -> Map String [Double]
mkValues s = foldr (\vals -> unionWith (++) (fromList $ map (fmap return) vals)) (M.fromSet (const []) s)

group :: Int -> [a] -> [[a]]
group _ [] = [[]]
group i xs = if null l
                then [f]
                else f : group i l
  where
    (f,l) = splitAt i xs

initSet :: [Named (Grouped [Named Double])] -> Set String
initSet = foldr (\(_,vals) -> S.union (S.fromList $ tkLibsName vals)) S.empty

tkLibsName :: Grouped [Named a] -> [String]
tkLibsName (Simple _ xs) = sort $ map fst xs
tkLibsName (Group xs) = maybe [] (tkLibsName . fst) $ uncons xs

