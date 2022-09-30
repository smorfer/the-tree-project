module Tree.Control.Transformation (transformTtoTFI, transformTFItoDT, transformTtoDT) where

import           Tree.Data.Definition           (Tree (..), TreeF (TreeF))
import           Tree.Data.Definition.DepthTree (DepthTree (..))


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
