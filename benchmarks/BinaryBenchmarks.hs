module BinaryBenchmarks where

import Criterion.Main
import qualified Curve.Binary.SECT113R1 as SECT113R1
import qualified Curve.Binary.SECT113R2 as SECT113R2
import qualified Curve.Binary.SECT131R1 as SECT131R1
import qualified Curve.Binary.SECT131R2 as SECT131R2
import qualified Curve.Binary.SECT163K1 as SECT163K1
import qualified Curve.Binary.SECT163R1 as SECT163R1
import qualified Curve.Binary.SECT163R2 as SECT163R2
import qualified Curve.Binary.SECT193R1 as SECT193R1
import qualified Curve.Binary.SECT193R2 as SECT193R2
import qualified Curve.Binary.SECT233K1 as SECT233K1
import qualified Curve.Binary.SECT233R1 as SECT233R1
import qualified Curve.Binary.SECT239K1 as SECT239K1
import qualified Curve.Binary.SECT283K1 as SECT283K1
import qualified Curve.Binary.SECT283R1 as SECT283R1
import qualified Curve.Binary.SECT409K1 as SECT409K1
import qualified Curve.Binary.SECT409R1 as SECT409R1
import qualified Curve.Binary.SECT571K1 as SECT571K1
import qualified Curve.Binary.SECT571R1 as SECT571R1

import GroupBenchmarks

benchmarkBinary :: Benchmark
benchmarkBinary = bgroup "Binary"
  [ benchmarkBinary' "SECT113R1" SECT113R1.gA SECT113R1.gP
  , benchmarkBinary' "SECT113R2" SECT113R2.gA SECT113R2.gP
  , benchmarkBinary' "SECT131R1" SECT131R1.gA SECT131R1.gP
  , benchmarkBinary' "SECT131R2" SECT131R2.gA SECT131R2.gP
  , benchmarkBinary' "SECT163K1" SECT163K1.gA SECT163K1.gP
  , benchmarkBinary' "SECT163R1" SECT163R1.gA SECT163R1.gP
  , benchmarkBinary' "SECT163R2" SECT163R2.gA SECT163R2.gP
  , benchmarkBinary' "SECT193R1" SECT193R1.gA SECT193R1.gP
  , benchmarkBinary' "SECT193R2" SECT193R2.gA SECT193R2.gP
  , benchmarkBinary' "SECT233K1" SECT233K1.gA SECT233K1.gP
  , benchmarkBinary' "SECT233R1" SECT233R1.gA SECT233R1.gP
  , benchmarkBinary' "SECT239K1" SECT239K1.gA SECT239K1.gP
  , benchmarkBinary' "SECT283K1" SECT283K1.gA SECT283K1.gP
  , benchmarkBinary' "SECT283R1" SECT283R1.gA SECT283R1.gP
  , benchmarkBinary' "SECT409K1" SECT409K1.gA SECT409K1.gP
  , benchmarkBinary' "SECT409R1" SECT409R1.gA SECT409R1.gP
  , benchmarkBinary' "SECT571K1" SECT571K1.gA SECT571K1.gP
  , benchmarkBinary' "SECT571R1" SECT571R1.gA SECT571R1.gP
  ]
  where
    benchmarkBinary' curve affine projective = bgroup curve
      [ benchmark "Affine" affine
      , benchmark "Projective" projective
      ]
