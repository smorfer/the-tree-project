module Tree.Control.Transformation (transformTtoTFI, transformTFItoDT, transformTtoDT, transformTFItoT, transformTtoST, transformTtoUT, treeableToDepthTree) where

import           Data.List                      (nubBy)
import           Tree.Data.Definition           (Tree (..), TreeF (TreeF))
import           Tree.Data.Definition.DepthTree (DepthTree (..))
import           Tree.Data.Definition.Treeable  (Treeable (..),
                                                 treeUnorderedEquality)


transformTtoTFI :: Tree -> TreeF Integer
transformTtoTFI = tfT2TFI 0
 where
  tfT2TFI :: Integer -> Tree -> TreeF Integer
  tfT2TFI n (Tree trs) = TreeF n (tfT2TFI (n + 1) <$> trs)

transformTFItoT :: TreeF Integer -> Tree
transformTFItoT (TreeF _ tfs) = Tree (transformTFItoT <$> tfs)

transformTFItoDT :: TreeF Integer -> DepthTree
transformTFItoDT = DepthTree . tfTFI2DT
 where
  tfTFI2DT :: TreeF Integer -> [Integer]
  tfTFI2DT (TreeF n tfs) = n : (tfTFI2DT =<< tfs)

transformTtoDT :: Tree -> DepthTree
transformTtoDT = transformTFItoDT . transformTtoTFI

treeableToDepthTree :: Treeable a => a -> DepthTree
treeableToDepthTree = DepthTree . treeableToDepthTree'

treeableToDepthTree' :: Treeable a => a -> [Integer]
treeableToDepthTree' t = case splitT t of
  [] -> [0]
  as -> 0 : ((+1) <$> concatMap treeableToDepthTree' as)

-- ST = Short Tree
transformTtoST :: Treeable t => t -> t
transformTtoST t = case splitT t of
  []   -> mergeT []
  [t'] -> transformTtoST t'
  ts   -> mergeT (transformTtoST <$> ts)
-- UT = Unique Tree
transformTtoUT :: Treeable t => t -> t
transformTtoUT t = case splitT t of
  []      -> mergeT []
  ts ->
    let transts = transformTtoUT <$> ts
    in mergeT (nubBy treeUnorderedEquality transts)
