module BinaryTests where

import Test.Tasty

import CurveTests
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

testBinary :: TestTree
testBinary = testGroup "Binary"
  [ test "SECT113R1" SECT113R1._g SECT113R1._h SECT113R1._n 2
  , test "SECT113R2" SECT113R2._g SECT113R2._h SECT113R2._n 2
  , test "SECT131R1" SECT131R1._g SECT131R1._h SECT131R1._n 2
  , test "SECT131R2" SECT131R2._g SECT131R2._h SECT131R2._n 2
  , test "SECT163K1" SECT163K1._g SECT163K1._h SECT163K1._n 2
  , test "SECT163R1" SECT163R1._g SECT163R1._h SECT163R1._n 2
  , test "SECT163R2" SECT163R2._g SECT163R2._h SECT163R2._n 2
  , test "SECT193R1" SECT193R1._g SECT193R1._h SECT193R1._n 2
  , test "SECT193R2" SECT193R2._g SECT193R2._h SECT193R2._n 2
  , test "SECT233K1" SECT233K1._g SECT233K1._h SECT233K1._n 2
  , test "SECT233R1" SECT233R1._g SECT233R1._h SECT233R1._n 2
  , test "SECT239K1" SECT239K1._g SECT239K1._h SECT239K1._n 2
  , test "SECT283K1" SECT283K1._g SECT283K1._h SECT283K1._n 2
  , test "SECT283R1" SECT283R1._g SECT283R1._h SECT283R1._n 2
  , test "SECT409K1" SECT409K1._g SECT409K1._h SECT409K1._n 2
  , test "SECT409R1" SECT409R1._g SECT409R1._h SECT409R1._n 2
  , test "SECT571K1" SECT571K1._g SECT571K1._h SECT571K1._n 2
  , test "SECT571R1" SECT571R1._g SECT571R1._h SECT571R1._n 2
  ]
