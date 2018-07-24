module BenchGraph.Render.Chart
  (
  mkChart
  )
where

import Control.Monad (void, mapM_, forM_)
import Data.List (uncons, sort, intercalate, nub)
import Data.Maybe (fromJust, mapMaybe)

import Graphics.Rendering.Chart.Easy hiding (uncons, colors, transform, bars)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Grid hiding (width)

import Data.Map.Strict (Map, unionWith, fromList, elems)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Monoid ((<>))

import BenchGraph.Render.Types
import BenchGraph.Named
import BenchGraph.Render.Common
import BenchGraph.Utils

mkChart :: String
        -- ^ The name of the benchs
        -> [(String,Int)]
        -- ^ The graphs used
        -> (Double -> String)
        -- ^ A show function
        -> ChartOutput
        -- ^ The format
        -> Either [Named (Grouped [Named Double])] [Named (Grouped [Named (Double, Double)])]
        -- ^ The data
        -> IO ()
mkChart _ _ _ _ (Left []) = return ()
mkChart _ _ _ _ (Right []) = return ()
mkChart title gparam s (ChartOutput filename chopt) grouped' =
  void $ renderableToFile fo (filename ++ "." ++ foExt) $ fillBackground def $ gridToRenderable grid
  where
    -- Group the benchs per line of 4 items
    grouped = either (map (fmap (fmap Left))) (map (fmap (fmap Right))) grouped'
    grp = group 4 grouped

    grid = wideAbove title' $ wideAbove graphsInfo $ wideAbove legend' $ aboveN $ map (besideN . map (layoutToGrid . set layout_legend Nothing . layout)) grp

      where
        -- Custom title
        title' = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre title
        ls = def { _font_size = 20 , _font_weight = FontWeightBold }

        -- Infor about graphs used
        graphsInfo = setPickFn nullPickFn $ label ls' HTA_Centre VTA_Centre $ (++) "Graphs used: " $ intercalate ", " $ map (\((n,f),xs) -> unwords [n,"with",show $ snd $ f $ last xs,"vertices"]) $ graphs True gparam
        ls' = def { _font_size = 15 , _font_weight = FontWeightNormal }

        -- Recreate the legend
        legend' = setPickFn nullPickFn $ toRenderable $ Legend legendStyle legendInfo
        hhgrp = Simple True "" $ Left $ zip (S.toList is) (replicate (S.size is) 0) -- False data to generate the legend
        legendStyle = fromJust $ _layout_legend $ layout ("",hhgrp)
        legendInfo = _plot_legend $ head $ _layout_plots $ layout ("",hhgrp)

    -- Render to svg
    (fo,foExt) = case chopt of
                   Png -> (def, "png")
                   Svg -> (def {_fo_format = SVG},"svg")

    layout e = layout_title .~ fst e
      $ layout_title_style . font_size .~ 10
      $ layout_x_axis . laxis_generate .~ autoIndexAxis (M.keys mapVal)
      $ layout_y_axis . laxis_override .~ (over axis_labels (\x -> [map (\(y,_) -> (y,s $ 10**(y - fromIntegral expo))) $ head x]) . axisGridHide ) -- Change the label with the corresponding 10 power
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ lay_plots
      $ def :: Layout PlotIndex Double

      where
        lay_plots = let plottedBars2 = plotBars bars2
                     in plottedBars2 : maybe [] (\stdVal -> [toPlotCustom (maybe 0 (M.size . snd) $ M.lookupMin mapVal) plottedBars2 $ barsErrs (map elems $ elems stdVal)]) std

        bars2 = plot_bars_titles .~ libsName
          $ plot_bars_values .~ addIndexes (map elems $ elems mapVal)
          $ plot_bars_singleton_width .~ 100
          $ plot_bars_style .~ BarsClustered
          $ plot_bars_spacing .~ BarsFixGap 30 5
          $ plot_bars_item_styles .~ map snd (filter (\(n,_) -> n `elem` libsName) colors)
          $ def
          where
            libsName = tkLibsName $ either id (map $ fmap fst) <$> snd e
            colors = zip (S.toList is)
              $ take (S.size is) $ map (\c -> (solidFillStyle c, Just (solidLine 1.0 $ opaque black))) (cycle defaultColorSeq)

        (expo,mapVal,std) = mkValue $ snd e

        mkValue x = (tenp, transformed, transformedStd) -- Make everyone > 1, so the log is positive.
          where
            mkAverageDeep  = M.map (M.filter (/=0) . M.map average) . M.map (mkValues is)
            doubleMap      = mkAverageDeep $ getSimplesWithG x
            stdVar         = mkAverageDeep <$> getSimplesStdWithG x
            maybeverysmall = concatMap elems $ elems doubleMap
            tenp           = if null maybeverysmall then 0 else 1 + ceiling (abs $ minimum $ map (logBase 10) maybeverysmall) :: Int
            transform u    = fromIntegral tenp + logBase 10 u
            transformed    = M.map (M.map transform) doubleMap
            transformedStd = M.mapWithKey
              (\k v -> let equiv  = fromJust (M.lookup k doubleMap)
                           equivt = fromJust (M.lookup k transformed)
                        in M.mapWithKey (\k' v' -> let d  = fromJust (M.lookup k' equiv)
                                                       dt = fromJust (M.lookup k' equivt)
                                                    in (transform (d-v'/2),dt,transform (d+v'/2))
              ) v) <$> stdVar

    is = either initSet initSet grouped'

    barsErrs v = plot_errbars_values .~ mkErrPts v
      $ plot_errbars_line_style .~ solidLine 1 (opaque black)
      $ plot_errbars_tick_length .~ 7
      $ def
      where
        mkErrPts = map (\(a,b,c) -> ErrPoint (ErrValue 0 0 0) (ErrValue a b c)) . concat -- ErrValue for x will be set after

-- | A render function very inspired by the one for bars BUT adapted for ErrBars
updateRender :: BarsPlotValue y => Int -> ([PlotIndex], b) -> PlotErrBars x y -> PointMapFn PlotIndex y -> BackendProgram ()
updateRender i allBarPoints p pmap = mapM_ clusteredErrBars vals
      where
        clusteredErrBars (x,ys) = forM_ (zip [0,1..] ys) $ \(j, y) -> drawErrBar0 p $ epmap (offset j) x y

        epmap xos x (ErrPoint _ (ErrValue yl y yh)) =
            ErrPoint (ErrValue 0 (x' + xos) 0) (ErrValue yl' y' yh') -- | To be used only with updateRenderer
            where (Point x' y')   = pmap' (x,y)
                  (Point _ yl') = pmap' (x,yl)
                  (Point _ yh') = pmap' (x,yh)

        offset j = fromIntegral (2*j-nys) * width/2 + width/2

        vals  = addIndexes $ group i $ _plot_errbars_values p
        width = max (minXInterval - 30) 5 / fromIntegral nys
        minXInterval = let diffs = zipWith (-) (tail mxs) mxs
                       in if null diffs
                            then 100
                            else minimum diffs
          where
            mxs = nub $ sort $ map mapX $ fst allBarPoints

        nys    = maximum [ length ys | (_,ys) <- vals ]
        pmap'  = mapXY pmap
        mapX x = p_x (pmap' (x,barsReference))

toPlotCustom ::(BarsPlotValue y) => Int -> Plot PlotIndex b -> PlotErrBars PlotIndex y -> Plot PlotIndex y
toPlotCustom i bars p = Plot {
  _plot_render     = updateRender i (_plot_all_points bars) p,
  _plot_legend     = [],
  _plot_all_points = ([],[]) -- do not need anything for scale
    }

mkValues :: Set String
         -- ^ This contains all the libs names. It allow to have a coherent legend
         -> [[Named Double]] -> Map String [Double]
mkValues s = foldr (\vals -> unionWith (++) (fromList $ map (fmap return) vals)) (M.fromSet (const []) s)

-- | Group a list by subList of length @i@
group :: Int -> [a] -> [[a]]
group _ [] = [[]]
group i xs = if null l
                then [f]
                else f : group i l
  where
    (f,l) = splitAt i xs

-- | Set that contains every libName, used to define which color will identify whom
initSet :: [Named (Grouped [Named a])] -> Set String
initSet = foldr (\(_,vals) -> S.union (S.fromList $ tkLibsName vals)) S.empty

tkLibsName :: Grouped [Named a] -> [String]
tkLibsName (Simple _ _ xs) = sort $ map fst xs
tkLibsName (Group xs) = maybe [] (tkLibsName . fst) $ uncons xs

getSimplesWithG :: Grouped (Either [Named Double] [Named (Double,Double)])  -> Map String [[Named Double]]
getSimplesWithG (Simple b n (Left v)) = if b then M.singleton n [v] else M.empty
getSimplesWithG (Simple b n (Right v)) = if b then M.singleton n [map (fmap fst) v] else M.empty
getSimplesWithG (Group lst) = M.unionsWith (++) $ map getSimplesWithG lst

getSimplesStdWithG :: Grouped (Either [Named Double] [Named (Double,Double)])  -> Maybe (Map String [[Named Double]])
getSimplesStdWithG (Simple _ _ Left{}) = Nothing
getSimplesStdWithG (Simple b n (Right v)) = if b then Just (M.singleton n [map (fmap snd) v]) else Nothing
getSimplesStdWithG (Group lst) = Just $ M.unionsWith (++) $ mapMaybe getSimplesStdWithG lst

-- | Copy/paste from http://hackage.haskell.org/package/Chart-1.9/docs/src/Graphics.Rendering.Chart.Plot.ErrBars.html#drawErrBar0 MODIFIED
drawErrBar0 :: PlotErrBars x y -> ErrPoint Double Double -> BackendProgram ()
drawErrBar0 ps (ErrPoint (ErrValue _ x _) (ErrValue yl _ yh)) = do
        let tl = _plot_errbars_tick_length ps
        withLineStyle (_plot_errbars_line_style ps) $
          strokePath $ moveTo' x yl
                    <> lineTo' x yh
                    <> moveTo' (x-tl) yl
                    <> lineTo' (x+tl) yl
                    <> moveTo' (x-tl) yh
                    <> lineTo' (x+tl) yh

