{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE RankNTypes        #-}

module Tree.Data.Definition (Tree(..), TreeF(..)) where

import           Tree.Data.Definition.Treeable (Treeable (..), TreeableF (..))
import           Tree.Data.Utils               (Fillable (..))

data Tree where
  Tree :: [Tree] -> Tree
  deriving Show

instance Treeable Tree where
  mergeT :: [Tree] -> Tree
  mergeT = Tree

  splitT :: Tree -> [Tree]
  splitT (Tree trs) = trs

data TreeF a where
  TreeF :: a -> [TreeF a] -> TreeF a

instance TreeableF TreeF where
  mergeTF :: Fillable a => [TreeF a] -> TreeF a
  mergeTF = TreeF fill
  splitTF :: TreeF a -> [TreeF a]
  splitTF (TreeF _ tfs) = tfs


instance Functor TreeF where
  fmap :: (a -> b) -> TreeF a -> TreeF b
  fmap f (TreeF a tfs) = TreeF (f a) (fmap (fmap f) tfs)

