module BinaryTests where

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
import Test.Tasty

import GroupTests

testBinary :: TestTree
testBinary = testGroup "Binary"
  [ testGroup "Affine"
    [ test "SECT113R1" SECT113R1.gA SECT113R1._h 2 SECT113R1._r
    , test "SECT113R2" SECT113R2.gA SECT113R2._h 2 SECT113R2._r
    , test "SECT131R1" SECT131R1.gA SECT131R1._h 2 SECT131R1._r
    , test "SECT131R2" SECT131R2.gA SECT131R2._h 2 SECT131R2._r
    , test "SECT163K1" SECT163K1.gA SECT163K1._h 2 SECT163K1._r
    , test "SECT163R1" SECT163R1.gA SECT163R1._h 2 SECT163R1._r
    , test "SECT163R2" SECT163R2.gA SECT163R2._h 2 SECT163R2._r
    , test "SECT193R1" SECT193R1.gA SECT193R1._h 2 SECT193R1._r
    , test "SECT193R2" SECT193R2.gA SECT193R2._h 2 SECT193R2._r
    , test "SECT233K1" SECT233K1.gA SECT233K1._h 2 SECT233K1._r
    , test "SECT233R1" SECT233R1.gA SECT233R1._h 2 SECT233R1._r
    , test "SECT239K1" SECT239K1.gA SECT239K1._h 2 SECT239K1._r
    , test "SECT283K1" SECT283K1.gA SECT283K1._h 2 SECT283K1._r
    , test "SECT283R1" SECT283R1.gA SECT283R1._h 2 SECT283R1._r
    , test "SECT409K1" SECT409K1.gA SECT409K1._h 2 SECT409K1._r
    , test "SECT409R1" SECT409R1.gA SECT409R1._h 2 SECT409R1._r
    , test "SECT571K1" SECT571K1.gA SECT571K1._h 2 SECT571K1._r
    , test "SECT571R1" SECT571R1.gA SECT571R1._h 2 SECT571R1._r
    ]
  ]
