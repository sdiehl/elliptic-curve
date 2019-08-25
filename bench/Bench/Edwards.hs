module Bench.Edwards where

import Criterion.Main
import qualified Data.Curve.Edwards.Curve1174  as Curve1174
import qualified Data.Curve.Edwards.Curve41417 as Curve41417
import qualified Data.Curve.Edwards.E222       as E222
import qualified Data.Curve.Edwards.E382       as E382
import qualified Data.Curve.Edwards.E521       as E521
import qualified Data.Curve.Edwards.Ed448      as Ed448
import qualified Data.Curve.Edwards.Ed3363     as Ed3363
import qualified Data.Curve.Edwards.Ed25519    as Ed25519
import qualified Data.Curve.Edwards.JubJub     as JubJub

import Bench.Curve

benchEdwards :: Benchmark
benchEdwards = bgroup "Edwards"
  [ benchEdwards'  "Curve1174"  Curve1174.gA  Curve1174.gP
  , benchEdwards' "Curve41417" Curve41417.gA Curve41417.gP
  , benchEdwards'       "E222"       E222.gA       E222.gP
  , benchEdwards'       "E382"       E382.gA       E382.gP
  , benchEdwards'       "E521"       E521.gA       E521.gP
  , benchEdwards'      "Ed448"      Ed448.gA      Ed448.gP
  , benchEdwards'     "Ed3363"     Ed3363.gA     Ed3363.gP
  , benchEdwards'    "Ed25519"    Ed25519.gA    Ed25519.gP
  , benchEdwards'     "JubJub"     JubJub.gA     JubJub.gP
  ]
  where
    benchEdwards' curve affine projective = bgroup curve
      [ benchmark "Affine" affine
      , benchmark "Projective" projective
      ]
