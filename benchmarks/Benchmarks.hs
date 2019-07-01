module Benchmarks where

import Protolude

import Criterion.Main

import Params
import Point

p1 :: Point
p1 = Point 0 1

p2 :: Point
p2 = Point 0 (-1)

benchmarks :: Benchmark
benchmarks = bgroup "JubJub"
  [ bench "Addition"
    $ whnf (uncurry affineAdd) (p1, p2)
  , bench "Doubling"
    $ whnf affineDouble p1
  , bench "Multiplication"
    $ whnf (flip affineMultiply 42) p1
  ]
