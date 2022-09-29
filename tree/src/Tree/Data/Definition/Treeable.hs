{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
module Tree.Data.Definition.Treeable ( Treeable(..), TreeableF(..)) where
import           Tree.Data.Utils (Fillable)

class Treeable t where
  mergeT :: [t] -> t
  splitT :: t -> [t]

class TreeableF tf where
  splitTF :: (Fillable a) => tf a -> [tf a]
  mergeTF :: (Fillable a) => [tf a] -> tf a
