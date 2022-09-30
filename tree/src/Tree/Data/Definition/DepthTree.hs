{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE InstanceSigs #-}
module Tree.Data.Definition.DepthTree (DepthTree(..)) where
import           Data.List.Split               (split, startsWith)
import           Tree.Data.Definition.Treeable (Treeable (..))

newtype DepthTreeF a where
  DepthTreeF :: [a] -> DepthTreeF a
  deriving Functor via []

-- TODO: Use NonEmpty List
newtype DepthTree = DepthTree { getDT :: [Integer]}

instance Show DepthTree where
  show :: DepthTree -> String
  show = show . getDT



instance Treeable DepthTree where
  mergeT :: [DepthTree] -> DepthTree
  mergeT dts = DepthTree (0 : (succ <$> (getDT =<< dts)))
  splitT :: DepthTree -> [DepthTree]
  splitT (DepthTree [])       = error "splitT: DepthTree empty"
  splitT (DepthTree [0]) = []
  splitT (DepthTree (_ : is)) = DepthTree <$> (fmap pred <$> split (startsWith [1]) is )
