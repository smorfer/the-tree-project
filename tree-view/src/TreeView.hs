module TreeView (runTreeView) where
import           Graphics.Gloss   (Picture (Circle), black, color, display,
                                   white)
import           TreeView.Default (defaultDisplay)

runTreeView :: IO()
runTreeView = do
    display defaultDisplay black (color white $ Circle 50)
