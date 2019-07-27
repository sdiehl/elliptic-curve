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

import CurveBenchmarks

benchmarkEdwards :: Benchmark
benchmarkEdwards = bgroup "Edwards"
  [ benchmark  "Curve1174"  Curve1174._g
  , benchmark "Curve41417" Curve41417._g
  , benchmark       "E222"       E222._g
  , benchmark       "E382"       E382._g
  , benchmark       "E521"       E521._g
  , benchmark      "Ed448"      Ed448._g
  , benchmark     "Ed3363"     Ed3363._g
  , benchmark    "Ed25519"    Ed25519._g
  , benchmark     "JubJub"     JubJub._g
  ]
