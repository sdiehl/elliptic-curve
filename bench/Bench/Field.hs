module Bench.Field where

import Criterion.Main
import qualified Data.Group.Field.BN254TF as BN254TF

import Bench.Group

benchField :: Benchmark
benchField = bgroup "Field"
  [ benchmark "BN254TF" BN254TF._g
  ]
