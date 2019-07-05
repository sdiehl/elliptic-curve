module Main where

import Protolude

import Criterion.Main
import Curve
import qualified Curve.BinaryWeierstrass.SECT113R1 as SECT113R1
import qualified Curve.BinaryWeierstrass.SECT113R2 as SECT113R2
import qualified Curve.BinaryWeierstrass.SECT131R1 as SECT131R1
import qualified Curve.BinaryWeierstrass.SECT131R2 as SECT131R2
import qualified Curve.BinaryWeierstrass.SECT163K1 as SECT163K1
import qualified Curve.BinaryWeierstrass.SECT163R1 as SECT163R1
import qualified Curve.BinaryWeierstrass.SECT163R2 as SECT163R2
import qualified Curve.BinaryWeierstrass.SECT193R1 as SECT193R1
import qualified Curve.BinaryWeierstrass.SECT193R2 as SECT193R2
import qualified Curve.BinaryWeierstrass.SECT233K1 as SECT233K1
import qualified Curve.BinaryWeierstrass.SECT233R1 as SECT233R1
import qualified Curve.BinaryWeierstrass.SECT239K1 as SECT239K1
import qualified Curve.BinaryWeierstrass.SECT283K1 as SECT283K1
import qualified Curve.BinaryWeierstrass.SECT283R1 as SECT283R1
import qualified Curve.BinaryWeierstrass.SECT409K1 as SECT409K1
import qualified Curve.BinaryWeierstrass.SECT409R1 as SECT409R1
import qualified Curve.BinaryWeierstrass.SECT571K1 as SECT571K1
import qualified Curve.BinaryWeierstrass.SECT571R1 as SECT571R1
import qualified Curve.ShortWeierstrass.BLS12_381.G1 as BLS12_381.G1
import qualified Curve.ShortWeierstrass.BLS12_381.G2 as BLS12_381.G2
import qualified Curve.ShortWeierstrass.BN128.G1 as BN128.G1
import qualified Curve.ShortWeierstrass.BN128.G2 as BN128.G2
import qualified Curve.ShortWeierstrass.SECP112R1 as SECP112R1
import qualified Curve.ShortWeierstrass.SECP112R2 as SECP112R2
import qualified Curve.ShortWeierstrass.SECP128R1 as SECP128R1
import qualified Curve.ShortWeierstrass.SECP128R2 as SECP128R2
import qualified Curve.ShortWeierstrass.SECP160K1 as SECP160K1
import qualified Curve.ShortWeierstrass.SECP160R1 as SECP160R1
import qualified Curve.ShortWeierstrass.SECP160R2 as SECP160R2
import qualified Curve.ShortWeierstrass.SECP192K1 as SECP192K1
import qualified Curve.ShortWeierstrass.SECP192R1 as SECP192R1
import qualified Curve.ShortWeierstrass.SECP224K1 as SECP224K1
import qualified Curve.ShortWeierstrass.SECP224R1 as SECP224R1
import qualified Curve.ShortWeierstrass.SECP256K1 as SECP256K1
import qualified Curve.ShortWeierstrass.SECP256R1 as SECP256R1
import qualified Curve.ShortWeierstrass.SECP384R1 as SECP384R1
import qualified Curve.ShortWeierstrass.SECP512R1 as SECP512R1
import qualified Curve.TwistedEdwards.JubJub as JubJub
import GHC.Base

benchmark :: forall r c k . Curve r c k => String -> Point r c k -> Benchmark
benchmark curve point = let point' = double point in bgroup curve
  [ bench "Addition"
    $ whnf (uncurry (<>)) (point, point')
  , bench "Doubling"
    $ whnf double point
  , bench "Inversion"
    $ whnf inv point
  , bench "Multiplication"
    $ whnf (mul 100) point
  ]

main :: IO ()
main = defaultMain
  [ benchmark "SECT113R1" (SECT113R1._g :: SECT113R1.P)
  , benchmark "SECT113R2" (SECT113R2._g :: SECT113R2.P)
  , benchmark "SECT131R1" (SECT131R1._g :: SECT131R1.P)
  , benchmark "SECT131R2" (SECT131R2._g :: SECT131R2.P)
  , benchmark "SECT163K1" (SECT163K1._g :: SECT163K1.P)
  , benchmark "SECT163R1" (SECT163R1._g :: SECT163R1.P)
  , benchmark "SECT163R2" (SECT163R2._g :: SECT163R2.P)
  , benchmark "SECT193R1" (SECT193R1._g :: SECT193R1.P)
  , benchmark "SECT193R2" (SECT193R2._g :: SECT193R2.P)
  , benchmark "SECT233K1" (SECT233K1._g :: SECT233K1.P)
  , benchmark "SECT233R1" (SECT233R1._g :: SECT233R1.P)
  , benchmark "SECT239K1" (SECT239K1._g :: SECT239K1.P)
  , benchmark "SECT283K1" (SECT283K1._g :: SECT283K1.P)
  , benchmark "SECT283R1" (SECT283R1._g :: SECT283R1.P)
  , benchmark "SECT409K1" (SECT409K1._g :: SECT409K1.P)
  , benchmark "SECT409R1" (SECT409R1._g :: SECT409R1.P)
  , benchmark "SECT571K1" (SECT571K1._g :: SECT571K1.P)
  , benchmark "SECT571R1" (SECT571R1._g :: SECT571R1.P)
  , bgroup "BLS12-381"
    [ benchmark "G1" (BLS12_381.G1._g :: BLS12_381.G1.P)
    , benchmark "G2" (BLS12_381.G2._g :: BLS12_381.G2.P)
    ]
  , bgroup "BN128"
    [ benchmark "G1" (BN128.G1._g :: BN128.G1.P)
    , benchmark "G2" (BN128.G2._g :: BN128.G2.P)
    ]
  , benchmark "SECP112R1" (SECP112R1._g :: SECP112R1.P)
  , benchmark "SECP112R2" (SECP112R2._g :: SECP112R2.P)
  , benchmark "SECP128R1" (SECP128R1._g :: SECP128R1.P)
  , benchmark "SECP128R2" (SECP128R2._g :: SECP128R2.P)
  , benchmark "SECP160K1" (SECP160K1._g :: SECP160K1.P)
  , benchmark "SECP160R1" (SECP160R1._g :: SECP160R1.P)
  , benchmark "SECP160R2" (SECP160R2._g :: SECP160R2.P)
  , benchmark "SECP192K1" (SECP192K1._g :: SECP192K1.P)
  , benchmark "SECP192R1" (SECP192R1._g :: SECP192R1.P)
  , benchmark "SECP224K1" (SECP224K1._g :: SECP224K1.P)
  , benchmark "SECP224R1" (SECP224R1._g :: SECP224R1.P)
  , benchmark "SECP256K1" (SECP256K1._g :: SECP256K1.P)
  , benchmark "SECP256R1" (SECP256R1._g :: SECP256R1.P)
  , benchmark "SECP384R1" (SECP384R1._g :: SECP384R1.P)
  , benchmark "SECP512R1" (SECP512R1._g :: SECP512R1.P)
  , benchmark "JubJub" (JubJub._g :: JubJub.P)
  ]
