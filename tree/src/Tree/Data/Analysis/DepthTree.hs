module Tree.Data.Analysis.DepthTree (getWidthAtDepth) where
import           Tree.Data.Definition.DepthTree (DepthTree (getDT))

getWidthAtDepth :: Integer -> DepthTree -> Integer
getWidthAtDepth d = fromIntegral . length . filter (==d) . getDT
