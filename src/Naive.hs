module Naive (affineDoubleNaive, affineMultiplyNaive) where

import Protolude

import Params (_d)
import Point (PointA(..), affineAdd)

-- | Naive doubling
affineDoubleNaive :: PointA -> PointA
affineDoubleNaive (PointA x y) = PointA x' y'
  where
    x' = (x * y + x * y) / (1 + _d * x * x * y * y)
    y' = (y * y + x * x) / (1 - _d * x * x * y * y)

-- | Naive multiplication
affineMultiplyNaive :: Int -> PointA -> PointA
affineMultiplyNaive n = foldr affineAdd (PointA 0 1) . replicate n
