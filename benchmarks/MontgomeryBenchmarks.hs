module MontgomeryBenchmarks where

import Criterion.Main
import qualified Curve.Montgomery.Curve448    as Curve448
import qualified Curve.Montgomery.Curve25519  as Curve25519
import qualified Curve.Montgomery.Curve383187 as Curve383187
import qualified Curve.Montgomery.M221        as M221
import qualified Curve.Montgomery.M383        as M383
import qualified Curve.Montgomery.M511        as M511

import CurveBenchmarks

benchmarkMontgomery :: Benchmark
benchmarkMontgomery = bgroup "Montgomery"
  [ benchmark    "Curve448"    Curve448._g
  , benchmark  "Curve25519"  Curve25519._g
  , benchmark "Curve383187" Curve383187._g
  , benchmark        "M221"        M221._g
  , benchmark        "M383"        M383._g
  , benchmark        "M511"        M511._g
  ]
