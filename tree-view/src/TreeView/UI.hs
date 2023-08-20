module TreeView.UI (getStatsUI) where
import           Graphics.Gloss                (Picture (Text), pictures,
                                                translate)
import           Graphics.Gloss.Data.Picture   (scale)
import           Tree.Control.Transformation   (treeableToDepthTree)
import           Tree.Data.Analysis            (tDepth, tMass, tWidth)
import           Tree.Data.Definition.Treeable (Treeable)
import           TreeView.Default              (defaultScreenHeight,
                                                defaultScreenWidth)

getStatsUI :: Treeable t => t -> Picture
getStatsUI t = translate (fromIntegral $ 10 +(-defaultScreenWidth) `div` 2) (fromIntegral $ (-20) + defaultScreenHeight `div` 2) $ scale 0.15 0.15 $ pictures [Text ("Mass: " ++ show (tMass t)), translate 0 (-120) $ Text ("Width: " ++ show (tWidth t)), translate 0 (-240) $ Text ("Depth: " ++ show (tDepth t)),translate 0 (-360) $ Text ("DT: " ++ (show $ treeableToDepthTree t))]
