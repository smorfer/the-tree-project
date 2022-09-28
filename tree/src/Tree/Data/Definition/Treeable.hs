module Tree.Data.Definition.Treeable (Treeable(..)) where

class Treeable t where
  mergeT :: [t] -> t
  splitT :: t -> [t]