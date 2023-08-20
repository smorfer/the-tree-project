{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}

{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
module Tree.Data.Definition.Treeable ( Treeable(..), TreeableF(..), treeUnorderedEquality, treeOrderedEquality) where
import           Data.List       (deleteBy)
import           Tree.Data.Utils (Fillable)

class Treeable t where
  mergeT :: [t] -> t
  splitT :: t -> [t]

treeUnorderedEquality :: (Treeable t) => t -> t -> Bool
treeUnorderedEquality t t' =
  let st = splitT t
      st' = splitT t'
  in
    ((length st == length st')
    && (case st of
          []      -> True
          t4 : ts ->
            let rst' = deleteBy treeUnorderedEquality t4 st'
            in ((length rst' == length ts) && treeUnorderedEquality (mergeT ts) (mergeT rst'))))

treeOrderedEquality :: (Treeable t) => t -> t -> Bool
treeOrderedEquality t t' =
  let st = splitT t
      st' = splitT t'
  in ((length st == length st') && (case st of
    [] -> True
    t3 : ts -> case st' of
      [] -> error "Impossible case, lists are of equal length and non-empty, but second list still is empty"
      t5 : ts' -> treeOrderedEquality t3 t5 && treeOrderedEquality (mergeT ts) (mergeT ts')
    ))

class TreeableF tf where
  splitTF :: (Fillable a) => tf a -> [tf a]
  mergeTF :: (Fillable a) => [tf a] -> tf a
