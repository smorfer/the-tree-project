{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Tree.Data.Generation.DepthTree (genDTbyMass) where
import           Tree.Data.Definition.DepthTree (DepthTree (..))

extendDT :: [Integer] -> [[Integer]]
extendDT ts = reverse <$> ((uncurry  (:)) <$> (zip [1..succ . last $ ts] (replicate (fromInteger . succ . last $ ts) $ reverse ts)))

genDTbyMass :: Integer -> [DepthTree]
genDTbyMass n = DepthTree <$> genDTbyMass' n [[0]]
    where genDTbyMass' :: Integer -> [[Integer]] -> [[Integer]]
          genDTbyMass' 0 ts = ts
          genDTbyMass' i ts = genDTbyMass' (i-1) (extendDT =<< ts)
