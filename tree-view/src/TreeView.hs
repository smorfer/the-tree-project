module TreeView (runTreeView, runTreeGallery, runTreeViewer) where
import           Graphics.Gloss                       (Picture (Text), display,
                                                       pictures, play,
                                                       translate, white)
import           Graphics.Gloss.Interface.IO.Game     (Event, Key (..),
                                                       KeyState (Down),
                                                       Modifiers (ctrl),
                                                       SpecialKey (..))
import           Graphics.Gloss.Interface.IO.Interact (Event (..),
                                                       Modifiers (..))
import           Tree.Control.Transformation          (treeableToDepthTree)
import           Tree.Data.Definition.Treeable        (Treeable)
import           Tree.Data.Generation.DepthTree       (genDTbyMass)
import           TreeView.Default                     (defaultDisplay,
                                                       defaultScreenHeight,
                                                       defaultScreenWidth)
import           TreeView.TreeVisualization           (connectDTSimple)

runTreeView :: IO()
runTreeView = do
    display defaultDisplay white (connectDTSimple ((!! 6) $ genDTbyMass 6))

runTreeGallery :: IO ()
runTreeGallery = do
    play
      defaultDisplay
      white
      30
      (1,1 :: Integer,0 :: Integer)
      (\(m,l,ix) -> pictures
       [connectDTSimple $ genDTbyMass m !! (fromIntegral ix `mod` fromIntegral l)
       ,translate (-fromIntegral defaultScreenWidth / 2) (-fromIntegral defaultScreenHeight / 2) (Text (show ((ix `mod` l) +1)))])
      handleGalleryInput
      (const id)

runTreeViewer :: Treeable a => [a] -> IO ()
runTreeViewer t = do
  let tvis = connectDTSimple . treeableToDepthTree <$> t
  play defaultDisplay white 30 0 ((tvis !!) . (`mod` length tvis)) handleTreeViewerInput (const id)

handleTreeViewerInput :: Event -> Int -> Int
handleTreeViewerInput (EventKey (SpecialKey sk) Down modifier _) i =
  let config = if Down == ctrl modifier then 10 else if Down == alt modifier then 100 else 1
  in
  case sk of
  KeyUp    -> i+config
  KeyDown  -> i-config
  KeyLeft  -> i-config
  KeyRight -> i+config
  _        -> i
handleTreeViewerInput _ i = i

handleGalleryInput :: Event -> (Integer,Integer,Integer) -> (Integer,Integer,Integer)
handleGalleryInput (EventKey (SpecialKey t) Down _ _) (m, l, ix) = case t of
  KeyLeft  -> (m, l, ix + l - 1)
  KeyRight -> (m, l, ix + 1)
  KeyUp    -> (m + 1, fromIntegral $ length $ genDTbyMass (m + 1), 0)
  KeyDown  -> let m' = if m /= 1 then m - 1 else 1 in (m', fromIntegral $ length (genDTbyMass m'), 0)
  _        -> (m, l, ix)
handleGalleryInput _ m                                          = m
