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
  [ bgroup    "Curve448"
    [ benchmark "Affine"    Curve448.gA
    ]
  , bgroup  "Curve25519"
    [ benchmark "Affine"  Curve25519.gA
    ]
  , bgroup "Curve383187"
    [ benchmark "Affine" Curve383187.gA
    ]
  , bgroup        "M221"
    [ benchmark "Affine"        M221.gA
    ]
  , bgroup        "M383"
    [ benchmark "Affine"        M383.gA
    ]
  , bgroup        "M511"
    [ benchmark "Affine"        M511.gA
    ]
  ]
