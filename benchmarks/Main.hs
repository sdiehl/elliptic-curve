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
import qualified Curve.Montgomery.Curve448 as Curve448
import qualified Curve.Montgomery.Curve25519 as Curve25519
import qualified Curve.Montgomery.Curve383187 as Curve383187
import qualified Curve.Montgomery.M221 as M221
import qualified Curve.Montgomery.M383 as M383
import qualified Curve.Montgomery.M511 as M511
import qualified Curve.ShortWeierstrass.ANSSIFRP256V1 as ANSSIFRP256V1
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
import qualified Curve.ShortWeierstrass.SECP521R1 as SECP521R1
import qualified Curve.TwistedEdwards.Curve1174 as Curve1174
import qualified Curve.TwistedEdwards.Curve41417 as Curve41417
import qualified Curve.TwistedEdwards.E222 as E222
import qualified Curve.TwistedEdwards.E382 as E382
import qualified Curve.TwistedEdwards.E521 as E521
import qualified Curve.TwistedEdwards.Ed448 as Ed448
import qualified Curve.TwistedEdwards.Ed3363 as Ed3363
import qualified Curve.TwistedEdwards.Ed25519 as Ed25519
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
    $ whnf (mul 6) point
  ]

main :: IO ()
main = defaultMain
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
  , benchmark "Curve448" Curve448._g
  , benchmark "Curve25519" Curve25519._g
  , benchmark "Curve383187" Curve383187._g
  , benchmark "M-221" M221._g
  , benchmark "M-383" M383._g
  , benchmark "M-511" M511._g
  , benchmark "ANSSI-FRP256V1" ANSSIFRP256V1._g
  , bgroup "BLS12-381"
    [ benchmark "G1" BLS12_381.G1._g
    , benchmark "G2" BLS12_381.G2._g
    ]
  , bgroup "BN128"
    [ benchmark "G1" BN128.G1._g
    , benchmark "G2" BN128.G2._g
    ]
  , benchmark "SECP112R1" SECP112R1._g
  , benchmark "SECP112R2" SECP112R2._g
  , benchmark "SECP128R1" SECP128R1._g
  , benchmark "SECP128R2" SECP128R2._g
  , benchmark "SECP160K1" SECP160K1._g
  , benchmark "SECP160R1" SECP160R1._g
  , benchmark "SECP160R2" SECP160R2._g
  , benchmark "SECP192K1" SECP192K1._g
  , benchmark "SECP192R1" SECP192R1._g
  , benchmark "SECP224K1" SECP224K1._g
  , benchmark "SECP224R1" SECP224R1._g
  , benchmark "SECP256K1" SECP256K1._g
  , benchmark "SECP256R1" SECP256R1._g
  , benchmark "SECP384R1" SECP384R1._g
  , benchmark "SECP521R1" SECP521R1._g
  , benchmark "Curve1174" Curve1174._g
  , benchmark "Curve41417" Curve41417._g
  , benchmark "E-222" E222._g
  , benchmark "E-382" E382._g
  , benchmark "E-521" E521._g
  , benchmark "Ed448" Ed448._g
  , benchmark "Ed3363" Ed3363._g
  , benchmark "Ed25519" Ed25519._g
  , benchmark "JubJub" JubJub._g
  ]
