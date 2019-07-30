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
  [ bgroup "SECT113R1"
    [ benchmark "Affine" SECT113R1.gA
    ]
  , bgroup "SECT113R2"
    [ benchmark "Affine" SECT113R2.gA
    ]
  , bgroup "SECT131R1"
    [ benchmark "Affine" SECT131R1.gA
    ]
  , bgroup "SECT131R2"
    [ benchmark "Affine" SECT131R2.gA
    ]
  , bgroup "SECT163K1"
    [ benchmark "Affine" SECT163K1.gA
    ]
  , bgroup "SECT163R1"
    [ benchmark "Affine" SECT163R1.gA
    ]
  , bgroup "SECT163R2"
    [ benchmark "Affine" SECT163R2.gA
    ]
  , bgroup "SECT193R1"
    [ benchmark "Affine" SECT193R1.gA
    ]
  , bgroup "SECT193R2"
    [ benchmark "Affine" SECT193R2.gA
    ]
  , bgroup "SECT233K1"
    [ benchmark "Affine" SECT233K1.gA
    ]
  , bgroup "SECT233R1"
    [ benchmark "Affine" SECT233R1.gA
    ]
  , bgroup "SECT239K1"
    [ benchmark "Affine" SECT239K1.gA
    ]
  , bgroup "SECT283K1"
    [ benchmark "Affine" SECT283K1.gA
    ]
  , bgroup "SECT283R1"
    [ benchmark "Affine" SECT283R1.gA
    ]
  , bgroup "SECT409K1"
    [ benchmark "Affine" SECT409K1.gA
    ]
  , bgroup "SECT409R1"
    [ benchmark "Affine" SECT409R1.gA
    ]
  , bgroup "SECT571K1"
    [ benchmark "Affine" SECT571K1.gA
    ]
  , bgroup "SECT571R1"
    [ benchmark "Affine" SECT571R1.gA
    ]
  ]
