module Bench (benchmarks) where

import Protolude

import Criterion.Main

import Naive
import Point

test1 :: PointA
test1 = PointA 0 1

test2 :: PointA
test2 = PointA 0 (-1)

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
      $ whnf (affineMultiply 10) test1
    , bench "naive_two"
      $ whnf (affineMultiplyNaive 10) test2
    , bench "fast_two"
      $ whnf (affineMultiply 10) test2
    ]
  ]
