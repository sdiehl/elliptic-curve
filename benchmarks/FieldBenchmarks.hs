module FieldBenchmarks where

import Criterion.Main
import qualified Curve.Field.BN254TF as BN254TF

import CurveBenchmarks

benchmarkField :: Benchmark
benchmarkField = bgroup "Field"
  [ benchmark "BN254TF" BN254TF._g
  ]
