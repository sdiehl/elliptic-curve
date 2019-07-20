module WeierstrassTests where

import Test.Tasty

import CurveTests
import qualified Curve.Weierstrass.Anomalous       as Anomalous
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

testWeierstrass :: TestTree
testWeierstrass = testGroup "Weierstrass"
  [ test        "Anomalous"       Anomalous._g       Anomalous._h       Anomalous._n       Anomalous._p
  , test   "ANSSI-FRP256V1"   ANSSIFRP256V1._g   ANSSIFRP256V1._h   ANSSIFRP256V1._n   ANSSIFRP256V1._p
  , test        "BLS12-381"       BLS12_381._g       BLS12_381._h       BLS12_381._n       BLS12_381._p
  , test       "BLS12-381T"      BLS12_381T._g      BLS12_381T._h      BLS12_381T._n      BLS12_381T._p
  , test        "BLS48-581"       BLS48_581._g       BLS48_581._h       BLS48_581._n       BLS48_581._p
  , test       "BLS48-581T"      BLS48_581T._g      BLS48_581T._h      BLS48_581T._n      BLS48_581T._p
  , test            "BN224"           BN224._g           BN224._h           BN224._n           BN224._p
  , test            "BN254"           BN254._g           BN254._h           BN254._n           BN254._p
  , test           "BN254T"          BN254T._g          BN254T._h          BN254T._n          BN254T._p
  , test           "BN254A"          BN254A._g          BN254A._h          BN254A._n          BN254A._p
  , test          "BN254AT"         BN254AT._g         BN254AT._h         BN254AT._n         BN254AT._p
  , test           "BN254B"          BN254B._g          BN254B._h          BN254B._n          BN254B._p
  , test          "BN254BT"         BN254BT._g         BN254BT._h         BN254BT._n         BN254BT._p
  , test            "BN256"           BN256._g           BN256._h           BN256._n           BN256._p
  , test            "BN384"           BN384._g           BN384._h           BN384._n           BN384._p
  , test            "BN462"           BN462._g           BN462._h           BN462._n           BN462._p
  , test           "BN462T"          BN462T._g          BN462T._h          BN462T._n          BN462T._p
  , test            "BN512"           BN512._g           BN512._h           BN512._n           BN512._p
  , test "Brainpool-P160R1" BrainpoolP160R1._g BrainpoolP160R1._h BrainpoolP160R1._n BrainpoolP160R1._p
  , test "Brainpool-P160T1" BrainpoolP160T1._g BrainpoolP160T1._h BrainpoolP160T1._n BrainpoolP160T1._p
  , test "Brainpool-P192R1" BrainpoolP192R1._g BrainpoolP192R1._h BrainpoolP192R1._n BrainpoolP192R1._p
  , test "Brainpool-P192T1" BrainpoolP192T1._g BrainpoolP192T1._h BrainpoolP192T1._n BrainpoolP192T1._p
  , test "Brainpool-P224R1" BrainpoolP224R1._g BrainpoolP224R1._h BrainpoolP224R1._n BrainpoolP224R1._p
  , test "Brainpool-P224T1" BrainpoolP224T1._g BrainpoolP224T1._h BrainpoolP224T1._n BrainpoolP224T1._p
  , test "Brainpool-P256R1" BrainpoolP256R1._g BrainpoolP256R1._h BrainpoolP256R1._n BrainpoolP256R1._p
  , test "Brainpool-P256T1" BrainpoolP256T1._g BrainpoolP256T1._h BrainpoolP256T1._n BrainpoolP256T1._p
  , test "Brainpool-P320R1" BrainpoolP320R1._g BrainpoolP320R1._h BrainpoolP320R1._n BrainpoolP320R1._p
  , test "Brainpool-P320T1" BrainpoolP320T1._g BrainpoolP320T1._h BrainpoolP320T1._n BrainpoolP320T1._p
  , test "Brainpool-P384R1" BrainpoolP384R1._g BrainpoolP384R1._h BrainpoolP384R1._n BrainpoolP384R1._p
  , test "Brainpool-P384T1" BrainpoolP384T1._g BrainpoolP384T1._h BrainpoolP384T1._n BrainpoolP384T1._p
  , test "Brainpool-P512R1" BrainpoolP512R1._g BrainpoolP512R1._h BrainpoolP512R1._n BrainpoolP512R1._p
  , test "Brainpool-P512T1" BrainpoolP512T1._g BrainpoolP512T1._h BrainpoolP512T1._n BrainpoolP512T1._p
  , test        "SECP112R1"       SECP112R1._g       SECP112R1._h       SECP112R1._n       SECP112R1._p
  , test        "SECP112R2"       SECP112R2._g       SECP112R2._h       SECP112R2._n       SECP112R2._p
  , test        "SECP128R1"       SECP128R1._g       SECP128R1._h       SECP128R1._n       SECP128R1._p
  , test        "SECP128R2"       SECP128R2._g       SECP128R2._h       SECP128R2._n       SECP128R2._p
  , test        "SECP160K1"       SECP160K1._g       SECP160K1._h       SECP160K1._n       SECP160K1._p
  , test        "SECP160R1"       SECP160R1._g       SECP160R1._h       SECP160R1._n       SECP160R1._p
  , test        "SECP160R2"       SECP160R2._g       SECP160R2._h       SECP160R2._n       SECP160R2._p
  , test        "SECP192K1"       SECP192K1._g       SECP192K1._h       SECP192K1._n       SECP192K1._p
  , test        "SECP192R1"       SECP192R1._g       SECP192R1._h       SECP192R1._n       SECP192R1._p
  , test        "SECP224K1"       SECP224K1._g       SECP224K1._h       SECP224K1._n       SECP224K1._p
  , test        "SECP224R1"       SECP224R1._g       SECP224R1._h       SECP224R1._n       SECP224R1._p
  , test        "SECP256K1"       SECP256K1._g       SECP256K1._h       SECP256K1._n       SECP256K1._p
  , test        "SECP256R1"       SECP256R1._g       SECP256R1._h       SECP256R1._n       SECP256R1._p
  , test        "SECP384R1"       SECP384R1._g       SECP384R1._h       SECP384R1._n       SECP384R1._p
  , test        "SECP521R1"       SECP521R1._g       SECP521R1._h       SECP521R1._n       SECP521R1._p
  ]
