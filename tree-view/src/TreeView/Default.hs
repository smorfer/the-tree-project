module TreeView.Default (defaultDisplay, defaultVMargin, defaultHMargin) where
import           Graphics.Gloss (Display (..))

defaultTitle :: String
defaultTitle = "Tree Viewer"

defaultDisplay :: Display
defaultDisplay = InWindow defaultTitle (480,480) (0, 0)

defaultVMargin :: Int
defaultVMargin = 15

defaultHMargin :: Int
defaultHMargin = 10
