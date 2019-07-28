module FieldBenchmarks where

import Criterion.Main
import qualified Group.Field.BN254TF as BN254TF

import GroupBenchmarks

benchmarkField :: Benchmark
benchmarkField = bgroup "Field"
  [ benchmark "BN254TF" BN254TF._g
  ]
