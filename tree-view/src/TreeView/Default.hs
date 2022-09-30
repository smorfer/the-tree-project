module TreeView.Default (defaultDisplay) where
import           Graphics.Gloss (Display (..))

defaultTitle :: String
defaultTitle = "Tree Viewer"

defaultDisplay :: Display
defaultDisplay = InWindow defaultTitle (480,480) (0, 0)
