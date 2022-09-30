module Tree.Data.Analysis (tMass, tWidth, tDepth, dtDepth) where

import           Tree.Data.Definition.DepthTree (DepthTree (getDT))
import           Tree.Data.Definition.Treeable  (Treeable (..))

tMass :: Treeable t => t -> Integer
tMass t = 1 + sum (tMass <$> splitT t)

tWidth :: Treeable t => t -> Integer
tWidth t = case splitT t of
  [] -> 1
  ts -> sum (tWidth <$> ts)

-- The DepthTree representation is a guide-line to the depth definition of a general tree
tDepth :: Treeable t => t -> Integer
tDepth t = case splitT t of
  [] -> 0
  ts -> 1 + maximum (tDepth <$> ts)

-- This is observable, when looking at the special case of the DepthTree itself
dtDepth :: DepthTree -> Integer
dtDepth = maximum . getDT
