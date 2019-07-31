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
  [ benchmarkEdwards'  "Curve1174"  Curve1174.gA  Curve1174.gP
  , benchmarkEdwards' "Curve41417" Curve41417.gA Curve41417.gP
  , benchmarkEdwards'       "E222"       E222.gA       E222.gP
  , benchmarkEdwards'       "E382"       E382.gA       E382.gP
  , benchmarkEdwards'       "E521"       E521.gA       E521.gP
  , benchmarkEdwards'      "Ed448"      Ed448.gA      Ed448.gP
  , benchmarkEdwards'     "Ed3363"     Ed3363.gA     Ed3363.gP
  , benchmarkEdwards'    "Ed25519"    Ed25519.gA    Ed25519.gP
  , benchmarkEdwards'     "JubJub"     JubJub.gA     JubJub.gP
  ]
  where
    benchmarkEdwards' curve affine projective = bgroup curve
      [ benchmark "Affine" affine
      , benchmark "Projective" projective
      ]
