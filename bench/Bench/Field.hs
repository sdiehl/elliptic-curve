module Bench.Field where

import Criterion.Main
import qualified Data.Cyclic.Field.BN254TF as BN254TF

import Bench.Curve

benchField :: Benchmark
benchField = bgroup "Field"
  [ benchmark "BN254TF" BN254TF._g
  ]
