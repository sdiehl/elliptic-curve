module Bench (benchmarks) where

import Protolude

import Criterion.Main

import Params
import Point

-- | Naive doubling
affineDoubleNaive :: Point -> Point
affineDoubleNaive (Point x y) = Point x' y'
  where
    x' = (x * y + x * y) / (1 + _d * x * x * y * y)
    y' = (y * y + x * x) / (1 - _d * x * x * y * y)

-- | Naive multiplication
affineMultiplyNaive :: Int -> Point -> Point
affineMultiplyNaive = (.) (foldr affineAdd (Point 0 1)) . replicate

-- | Point of order one
test1 :: Point
test1 = Point 0 1

-- | Point of order two
test2 :: Point
test2 = Point 0 (-1)

-- | Benchmarks
benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "doubling"
    [ bench "naive_one"
      $ whnf affineDoubleNaive test1
    , bench "fast_one"
      $ whnf affineDouble test1
    , bench "naive_two"
      $ whnf affineDoubleNaive test2
    , bench "fast_two"
      $ whnf affineDouble test2
    ]
  , bgroup "multiplication"
    [ bench "naive_one"
      $ whnf (affineMultiplyNaive 10) test1
    , bench "fast_one"
      $ whnf (flip affineMultiply 10) test1
    , bench "naive_two"
      $ whnf (affineMultiplyNaive 10) test2
    , bench "fast_two"
      $ whnf (flip affineMultiply 10) test2
    ]
  ]
