module EdwardsBenchmarks where

import Criterion.Main
import qualified Curve.Edwards.Curve1174  as Curve1174
import qualified Curve.Edwards.Curve41417 as Curve41417
import qualified Curve.Edwards.E222       as E222
import qualified Curve.Edwards.E382       as E382
import qualified Curve.Edwards.E521       as E521
import qualified Curve.Edwards.Ed448      as Ed448
import qualified Curve.Edwards.Ed3363     as Ed3363
import qualified Curve.Edwards.Ed25519    as Ed25519
import qualified Curve.Edwards.JubJub     as JubJub

import GroupBenchmarks

benchmarkEdwards :: Benchmark
benchmarkEdwards = bgroup "Edwards"
  [ bgroup  "Curve1174"
    [ benchmark "Affine"  Curve1174.gA
    ]
  , bgroup "Curve41417"
    [ benchmark "Affine" Curve41417.gA
    ]
  , bgroup       "E222"
    [ benchmark "Affine"       E222.gA
    ]
  , bgroup       "E382"
    [ benchmark "Affine"       E382.gA
    ]
  , bgroup       "E521"
    [ benchmark "Affine"       E521.gA
    ]
  , bgroup      "Ed448"
    [ benchmark "Affine"      Ed448.gA
    ]
  , bgroup     "Ed3363"
    [ benchmark "Affine"     Ed3363.gA
    ]
  , bgroup    "Ed25519"
    [ benchmark "Affine"    Ed25519.gA
    ]
  , bgroup     "JubJub"
    [ benchmark "Affine"     JubJub.gA
    ]
  ]
