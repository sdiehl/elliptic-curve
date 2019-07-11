module Main where

import Protolude

import Criterion.Main
import Curve
import qualified Curve.Binary.SECT113R1            as SECT113R1
import qualified Curve.Binary.SECT113R2            as SECT113R2
import qualified Curve.Binary.SECT131R1            as SECT131R1
import qualified Curve.Binary.SECT131R2            as SECT131R2
import qualified Curve.Binary.SECT163K1            as SECT163K1
import qualified Curve.Binary.SECT163R1            as SECT163R1
import qualified Curve.Binary.SECT163R2            as SECT163R2
import qualified Curve.Binary.SECT193R1            as SECT193R1
import qualified Curve.Binary.SECT193R2            as SECT193R2
import qualified Curve.Binary.SECT233K1            as SECT233K1
import qualified Curve.Binary.SECT233R1            as SECT233R1
import qualified Curve.Binary.SECT239K1            as SECT239K1
import qualified Curve.Binary.SECT283K1            as SECT283K1
import qualified Curve.Binary.SECT283R1            as SECT283R1
import qualified Curve.Binary.SECT409K1            as SECT409K1
import qualified Curve.Binary.SECT409R1            as SECT409R1
import qualified Curve.Binary.SECT571K1            as SECT571K1
import qualified Curve.Binary.SECT571R1            as SECT571R1
import qualified Curve.Edwards.Curve1174           as Curve1174
import qualified Curve.Edwards.Curve41417          as Curve41417
import qualified Curve.Edwards.E222                as E222
import qualified Curve.Edwards.E382                as E382
import qualified Curve.Edwards.E521                as E521
import qualified Curve.Edwards.Ed448               as Ed448
import qualified Curve.Edwards.Ed3363              as Ed3363
import qualified Curve.Edwards.Ed25519             as Ed25519
import qualified Curve.Edwards.JubJub              as JubJub
import qualified Curve.Montgomery.Curve448         as Curve448
import qualified Curve.Montgomery.Curve25519       as Curve25519
import qualified Curve.Montgomery.Curve383187      as Curve383187
import qualified Curve.Montgomery.M221             as M221
import qualified Curve.Montgomery.M383             as M383
import qualified Curve.Montgomery.M511             as M511
import qualified Curve.Weierstrass.ANSSIFRP256V1   as ANSSIFRP256V1
import qualified Curve.Weierstrass.BLS12_381       as BLS12_381
import qualified Curve.Weierstrass.BLS12_381T      as BLS12_381T
import qualified Curve.Weierstrass.BLS48_581       as BLS48_581
import qualified Curve.Weierstrass.BLS48_581T      as BLS48_581T
import qualified Curve.Weierstrass.BN224           as BN224
import qualified Curve.Weierstrass.BN254           as BN254
import qualified Curve.Weierstrass.BN254T          as BN254T
import qualified Curve.Weierstrass.BN254A          as BN254A
import qualified Curve.Weierstrass.BN254AT         as BN254AT
import qualified Curve.Weierstrass.BN254B          as BN254B
import qualified Curve.Weierstrass.BN254BT         as BN254BT
import qualified Curve.Weierstrass.BN256           as BN256
import qualified Curve.Weierstrass.BN384           as BN384
import qualified Curve.Weierstrass.BN462           as BN462
import qualified Curve.Weierstrass.BN462T          as BN462T
import qualified Curve.Weierstrass.BN512           as BN512
import qualified Curve.Weierstrass.BrainpoolP160R1 as BrainpoolP160R1
import qualified Curve.Weierstrass.BrainpoolP160T1 as BrainpoolP160T1
import qualified Curve.Weierstrass.BrainpoolP192R1 as BrainpoolP192R1
import qualified Curve.Weierstrass.BrainpoolP192T1 as BrainpoolP192T1
import qualified Curve.Weierstrass.BrainpoolP224R1 as BrainpoolP224R1
import qualified Curve.Weierstrass.BrainpoolP224T1 as BrainpoolP224T1
import qualified Curve.Weierstrass.BrainpoolP256R1 as BrainpoolP256R1
import qualified Curve.Weierstrass.BrainpoolP256T1 as BrainpoolP256T1
import qualified Curve.Weierstrass.BrainpoolP320R1 as BrainpoolP320R1
import qualified Curve.Weierstrass.BrainpoolP320T1 as BrainpoolP320T1
import qualified Curve.Weierstrass.BrainpoolP384R1 as BrainpoolP384R1
import qualified Curve.Weierstrass.BrainpoolP384T1 as BrainpoolP384T1
import qualified Curve.Weierstrass.BrainpoolP512R1 as BrainpoolP512R1
import qualified Curve.Weierstrass.BrainpoolP512T1 as BrainpoolP512T1
import qualified Curve.Weierstrass.SECP112R1       as SECP112R1
import qualified Curve.Weierstrass.SECP112R2       as SECP112R2
import qualified Curve.Weierstrass.SECP128R1       as SECP128R1
import qualified Curve.Weierstrass.SECP128R2       as SECP128R2
import qualified Curve.Weierstrass.SECP160K1       as SECP160K1
import qualified Curve.Weierstrass.SECP160R1       as SECP160R1
import qualified Curve.Weierstrass.SECP160R2       as SECP160R2
import qualified Curve.Weierstrass.SECP192K1       as SECP192K1
import qualified Curve.Weierstrass.SECP192R1       as SECP192R1
import qualified Curve.Weierstrass.SECP224K1       as SECP224K1
import qualified Curve.Weierstrass.SECP224R1       as SECP224R1
import qualified Curve.Weierstrass.SECP256K1       as SECP256K1
import qualified Curve.Weierstrass.SECP256R1       as SECP256R1
import qualified Curve.Weierstrass.SECP384R1       as SECP384R1
import qualified Curve.Weierstrass.SECP521R1       as SECP521R1
import GHC.Base

benchmark :: forall r c k . Curve r c k => String -> Point r c k -> Benchmark
benchmark = (. whnf (mul (6 :: Int))) . bench

main :: IO ()
main = defaultMain
  [ bgroup "Binary"
    [ benchmark        "SECT113R1"       SECT113R1._g
    , benchmark        "SECT113R2"       SECT113R2._g
    , benchmark        "SECT131R1"       SECT131R1._g
    , benchmark        "SECT131R2"       SECT131R2._g
    , benchmark        "SECT163K1"       SECT163K1._g
    , benchmark        "SECT163R1"       SECT163R1._g
    , benchmark        "SECT163R2"       SECT163R2._g
    , benchmark        "SECT193R1"       SECT193R1._g
    , benchmark        "SECT193R2"       SECT193R2._g
    , benchmark        "SECT233K1"       SECT233K1._g
    , benchmark        "SECT233R1"       SECT233R1._g
    , benchmark        "SECT239K1"       SECT239K1._g
    , benchmark        "SECT283K1"       SECT283K1._g
    , benchmark        "SECT283R1"       SECT283R1._g
    , benchmark        "SECT409K1"       SECT409K1._g
    , benchmark        "SECT409R1"       SECT409R1._g
    , benchmark        "SECT571K1"       SECT571K1._g
    , benchmark        "SECT571R1"       SECT571R1._g
    ]
  , bgroup "Edwards"
    [ benchmark        "Curve1174"       Curve1174._g
    , benchmark       "Curve41417"      Curve41417._g
    , benchmark            "E-222"            E222._g
    , benchmark            "E-382"            E382._g
    , benchmark            "E-521"            E521._g
    , benchmark            "Ed448"           Ed448._g
    , benchmark           "Ed3363"          Ed3363._g
    , benchmark          "Ed25519"         Ed25519._g
    , benchmark           "JubJub"          JubJub._g
    ]
  , bgroup "Montgomery"
    [ benchmark         "Curve448"        Curve448._g
    , benchmark       "Curve25519"      Curve25519._g
    , benchmark      "Curve383187"     Curve383187._g
    , benchmark            "M-221"            M221._g
    , benchmark            "M-383"            M383._g
    , benchmark            "M-511"            M511._g
    ]
  , bgroup "Weierstrass"
    [ benchmark   "ANSSI-FRP256V1"   ANSSIFRP256V1._g
    , benchmark        "BLS12-381"       BLS12_381._g
    , benchmark       "BLS12-381T"      BLS12_381T._g
    , benchmark        "BLS48-581"       BLS48_581._g
    , benchmark       "BLS48-581T"      BLS48_581T._g
    , benchmark            "BN224"           BN224._g
    , benchmark            "BN254"           BN254._g
    , benchmark           "BN254T"          BN254T._g
    , benchmark           "BN254A"          BN254A._g
    , benchmark          "BN254AT"         BN254AT._g
    , benchmark           "BN254B"          BN254B._g
    , benchmark          "BN254BT"         BN254BT._g
    , benchmark            "BN256"           BN256._g
    , benchmark            "BN384"           BN384._g
    , benchmark            "BN462"           BN462._g
    , benchmark           "BN462T"          BN462T._g
    , benchmark            "BN512"           BN512._g
    , benchmark "Brainpool-P160R1" BrainpoolP160R1._g
    , benchmark "Brainpool-P160T1" BrainpoolP160T1._g
    , benchmark "Brainpool-P192R1" BrainpoolP192R1._g
    , benchmark "Brainpool-P192T1" BrainpoolP192T1._g
    , benchmark "Brainpool-P224R1" BrainpoolP224R1._g
    , benchmark "Brainpool-P224T1" BrainpoolP224T1._g
    , benchmark "Brainpool-P256R1" BrainpoolP256R1._g
    , benchmark "Brainpool-P256T1" BrainpoolP256T1._g
    , benchmark "Brainpool-P320R1" BrainpoolP320R1._g
    , benchmark "Brainpool-P320T1" BrainpoolP320T1._g
    , benchmark "Brainpool-P384R1" BrainpoolP384R1._g
    , benchmark "Brainpool-P384T1" BrainpoolP384T1._g
    , benchmark "Brainpool-P512R1" BrainpoolP512R1._g
    , benchmark "Brainpool-P512T1" BrainpoolP512T1._g
    , benchmark        "SECP112R1"       SECP112R1._g
    , benchmark        "SECP112R2"       SECP112R2._g
    , benchmark        "SECP128R1"       SECP128R1._g
    , benchmark        "SECP128R2"       SECP128R2._g
    , benchmark        "SECP160K1"       SECP160K1._g
    , benchmark        "SECP160R1"       SECP160R1._g
    , benchmark        "SECP160R2"       SECP160R2._g
    , benchmark        "SECP192K1"       SECP192K1._g
    , benchmark        "SECP192R1"       SECP192R1._g
    , benchmark        "SECP224K1"       SECP224K1._g
    , benchmark        "SECP224R1"       SECP224R1._g
    , benchmark        "SECP256K1"       SECP256K1._g
    , benchmark        "SECP256R1"       SECP256R1._g
    , benchmark        "SECP384R1"       SECP384R1._g
    , benchmark        "SECP521R1"       SECP521R1._g
    ]
  ]
