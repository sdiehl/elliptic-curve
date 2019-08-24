module Bench.Montgomery where

import Criterion.Main
import qualified Curve.Montgomery.Curve448    as Curve448
import qualified Curve.Montgomery.Curve25519  as Curve25519
import qualified Curve.Montgomery.Curve383187 as Curve383187
import qualified Curve.Montgomery.M221        as M221
import qualified Curve.Montgomery.M383        as M383
import qualified Curve.Montgomery.M511        as M511

import Bench.Group

benchMontgomery :: Benchmark
benchMontgomery = bgroup "Montgomery"
  [ benchMontgomery'    "Curve448"    Curve448.gA
  , benchMontgomery'  "Curve25519"  Curve25519.gA
  , benchMontgomery' "Curve383187" Curve383187.gA
  , benchMontgomery'        "M221"        M221.gA
  , benchMontgomery'        "M383"        M383.gA
  , benchMontgomery'        "M511"        M511.gA
  ]
  where
    benchMontgomery' curve affine = bgroup curve
      [ benchmark "Affine" affine
      ]
