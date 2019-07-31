module MontgomeryBenchmarks where

import Criterion.Main
import qualified Curve.Montgomery.Curve448    as Curve448
import qualified Curve.Montgomery.Curve25519  as Curve25519
import qualified Curve.Montgomery.Curve383187 as Curve383187
import qualified Curve.Montgomery.M221        as M221
import qualified Curve.Montgomery.M383        as M383
import qualified Curve.Montgomery.M511        as M511

import GroupBenchmarks

benchmarkMontgomery :: Benchmark
benchmarkMontgomery = bgroup "Montgomery"
  [ benchmarkMontgomery'    "Curve448"    Curve448.gA
  , benchmarkMontgomery'  "Curve25519"  Curve25519.gA
  , benchmarkMontgomery' "Curve383187" Curve383187.gA
  , benchmarkMontgomery'        "M221"        M221.gA
  , benchmarkMontgomery'        "M383"        M383.gA
  , benchmarkMontgomery'        "M511"        M511.gA
  ]
  where
    benchmarkMontgomery' curve affine = bgroup curve
      [ benchmark "Affine" affine
      ]
