{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Tree.Data.Definition (Tree(..), TreeF(..)) where

import           Tree.Data.Definition.Treeable (Treeable(..))

data Tree where
  Tree :: [Tree] -> Tree
  deriving Show

data TreeF a where
  TreeF :: a -> [TreeF a] -> TreeF a

instance Functor TreeF where
  fmap :: (a -> b) -> TreeF a -> TreeF b
  fmap f (TreeF a tfs) = TreeF (f a) (fmap (fmap f) tfs)

instance Treeable Tree where
  mergeT :: [Tree] -> Tree
  mergeT = Tree

  splitT :: Tree -> [Tree]
  splitT (Tree trs) = trs
