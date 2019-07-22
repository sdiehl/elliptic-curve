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

import CurveBenchmarks

benchmarkBinary :: Benchmark
benchmarkBinary = bgroup "Binary"
  [ benchmark "SECT113R1" SECT113R1._g
  , benchmark "SECT113R2" SECT113R2._g
  , benchmark "SECT131R1" SECT131R1._g
  , benchmark "SECT131R2" SECT131R2._g
  , benchmark "SECT163K1" SECT163K1._g
  , benchmark "SECT163R1" SECT163R1._g
  , benchmark "SECT163R2" SECT163R2._g
  , benchmark "SECT193R1" SECT193R1._g
  , benchmark "SECT193R2" SECT193R2._g
  , benchmark "SECT233K1" SECT233K1._g
  , benchmark "SECT233R1" SECT233R1._g
  , benchmark "SECT239K1" SECT239K1._g
  , benchmark "SECT283K1" SECT283K1._g
  , benchmark "SECT283R1" SECT283R1._g
  , benchmark "SECT409K1" SECT409K1._g
  , benchmark "SECT409R1" SECT409R1._g
  , benchmark "SECT571K1" SECT571K1._g
  , benchmark "SECT571R1" SECT571R1._g
  ]
