module TreeView.Default (defaultDisplay, defaultVMargin, defaultHMargin, defaultVOffset, defaultHOffset, defaultRoot, defaultScreenHeight, defaultScreenWidth) where
import           Graphics.Gloss (Display (..), Point)

defaultTitle :: String
defaultTitle = "Tree Viewer"

defaultUIWidth :: Int
defaultUIWidth = 100

defaultScreenHeight :: Int
defaultScreenHeight = 720

defaultScreenWidth :: Int
defaultScreenWidth = 720+defaultUIWidth

defaultDisplay :: Display
defaultDisplay = InWindow defaultTitle (defaultScreenWidth,defaultScreenHeight) (0, 0)

defaultVMargin :: Float
defaultVMargin = -40 * 1.61803398875

defaultHMargin :: Float
defaultHMargin = -40

defaultVOffset :: Float
defaultVOffset = -200

defaultHOffset :: Float
defaultHOffset = 0

defaultRoot :: Point
defaultRoot = (defaultHOffset, defaultVOffset)
