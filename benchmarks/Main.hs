module Main where

import Protolude

import Criterion.Main

import Curve
import qualified JubJub as JubJub

p1 :: JubJub.Point
p1 = JubJub.A 0 1

p2 :: JubJub.Point
p2 = JubJub.A 0 (-1)

main :: IO ()
main = defaultMain
  [ bench "Addition"
    $ whnf (uncurry (<>)) (p1, p2)
  , bench "Doubling"
    $ whnf double p1
  , bench "Multiplication"
    $ whnf (scale 42) p1
  ]
