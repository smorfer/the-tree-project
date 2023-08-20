{-# LANGUAGE TupleSections #-}
module TreeView.TreeVisualization (getDTConnectorPoints, getConnectorPointsByDepth, connectDTSimple) where
import           Graphics.Gloss                 (Picture, Point, line, pictures)
import           Tree.Data.Analysis             (dtDepth)
import           Tree.Data.Analysis.DepthTree   (getWidthAtDepth)
import           Tree.Data.Definition.DepthTree (DepthTree (..))
import           Tree.Data.Definition.Treeable  (Treeable (..))
import           TreeView.Default               (defaultHMargin, defaultHOffset,
                                                 defaultRoot, defaultVMargin,
                                                 defaultVOffset)
import           TreeView.UI                    (getStatsUI)

getConnectorPointsByDepth :: DepthTree -> Integer -> [Point]
getConnectorPointsByDepth dt d =
    let width = getWidthAtDepth d dt
        preix = fromIntegral <$> [1..width `div` 2]
        ix = if even width
        then reverse ((*(-1)) <$> preix) ++ preix
        else reverse ((*(-1)) <$> preix) ++ (0:preix)
    in (,defaultVOffset - fromIntegral d*defaultVMargin) . (+ defaultHOffset) . (*defaultHMargin) <$> ix

getDTConnectorPoints :: DepthTree -> [[Point]]
getDTConnectorPoints dt = [defaultRoot] : (getConnectorPointsByDepth dt <$> [1..(dtDepth dt)])

getDTLengths :: [DepthTree] -> [Integer]
getDTLengths dts = fromIntegral . length <$> (splitT <$> dts)

connectDTLayer :: [Point] -> [Integer] -> [Point] -> [Picture]
connectDTLayer [] _ _ = []
connectDTLayer (_ : _) [] _ = []
connectDTLayer (x0 : x1) (n : ns) ptss = (line <$> ((x0:) . (:[]) <$> take (fromIntegral n) ptss)) ++ connectDTLayer x1 ns (drop (fromIntegral n) ptss)

connectDTSimple :: DepthTree -> Picture
connectDTSimple dt =
    let cts = getDTConnectorPoints dt
    in pictures $ connectDTSimple' (head cts) [dt] (tail cts) ++ [getStatsUI dt]

connectDTSimple' :: [Point] -> [DepthTree] -> [[Point]] -> [Picture]
connectDTSimple' _ _ [] = []
connectDTSimple' current dts (x0 : x1) = connectDTLayer current (getDTLengths dts) x0 ++ connectDTSimple' x0 (concatMap splitT dts) x1

-- connectDTSimple :: [DepthTree] -> [Point] -> [[Point]] -> [Picture]
-- connectDTSimple dts refs pts = let dtss = splitT <$> dts in case pts of
--                                  [] -> []
--                                  (x0 : x1) -> connectDTLayer refs (fromIntegral . length <$> dtss) x0 ++ connectDTSimple (concat dtss) x0 x1
