module FieldBenchmarks where

import Protolude

import Criterion.Main
import Curve.Field
import qualified Curve.Field.BN254TF as BN254TF

import CurveBenchmarks

benchmarkField :: Benchmark
benchmarkField = bgroup "Field"
  [ benchmark "BN254TF" (witness :: FGroup BN254TF.Fp12)
  ]
