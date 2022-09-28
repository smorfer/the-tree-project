module Tree.Control.Transformation (transformTtoTFI, transformTFItoDT, transformTtoDT) where

import           Tree.Data.Definition.DepthTree (DepthTree(..))
import           Tree.Data.Definition (Tree(..), TreeF(TreeF))

tfT2TFI :: Integer -> Tree -> TreeF Integer
tfT2TFI n (Tree trs) = TreeF n (tfT2TFI (n + 1) <$> trs)

transformTtoTFI :: Tree -> TreeF Integer
transformTtoTFI = tfT2TFI 0

tfTFI2DT :: TreeF Integer -> [Integer]
tfTFI2DT (TreeF n tfs) = n:(tfTFI2DT =<< tfs)

transformTFItoDT :: TreeF Integer -> DepthTree
transformTFItoDT = DepthTree . tfTFI2DT

transformTtoDT :: Tree -> DepthTree
transformTtoDT = transformTFItoDT . transformTtoTFI