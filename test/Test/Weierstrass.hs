module Test.Weierstrass where

import Protolude

import qualified Curve.Weierstrass.Anomalous       as Anomalous
import qualified Curve.Weierstrass.ANSSIFRP256V1   as ANSSIFRP256V1
import qualified Curve.Weierstrass.BLS12381        as BLS12381
import qualified Curve.Weierstrass.BLS12381T       as BLS12381T
import qualified Curve.Weierstrass.BLS48581        as BLS48581
import qualified Curve.Weierstrass.BLS48581T       as BLS48581T
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
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Group

testWeierstrass :: TestTree
testWeierstrass = testGroup "Weierstrass"
  [ testWeierstrass' 100       "Anomalous"       Anomalous._h       Anomalous._q       Anomalous._r       Anomalous.gA       Anomalous.gJ       Anomalous.gP
  , testWeierstrass' 100   "ANSSIFRP256V1"   ANSSIFRP256V1._h   ANSSIFRP256V1._q   ANSSIFRP256V1._r   ANSSIFRP256V1.gA   ANSSIFRP256V1.gJ   ANSSIFRP256V1.gP
  , testWeierstrass' 100        "BLS12381"        BLS12381._h        BLS12381._q        BLS12381._r        BLS12381.gA        BLS12381.gJ        BLS12381.gP
  , testWeierstrass'  10       "BLS12381T"       BLS12381T._h       BLS12381T._q       BLS12381T._r       BLS12381T.gA       BLS12381T.gJ       BLS12381T.gP
  , testWeierstrass' 100        "BLS48581"        BLS48581._h        BLS48581._q        BLS48581._r        BLS48581.gA        BLS48581.gJ        BLS48581.gP
  , testWeierstrass'   0       "BLS48581T"       BLS48581T._h       BLS48581T._q       BLS48581T._r       BLS48581T.gA       BLS48581T.gJ       BLS48581T.gP
  , testWeierstrass' 100           "BN224"           BN224._h           BN224._q           BN224._r           BN224.gA           BN224.gJ           BN224.gP
  , testWeierstrass' 100           "BN254"           BN254._h           BN254._q           BN254._r           BN254.gA           BN254.gJ           BN254.gP
  , testWeierstrass'  10          "BN254T"          BN254T._h          BN254T._q          BN254T._r          BN254T.gA          BN254T.gJ          BN254T.gP
  , testWeierstrass' 100          "BN254A"          BN254A._h          BN254A._q          BN254A._r          BN254A.gA          BN254A.gJ          BN254A.gP
  , testWeierstrass'  10         "BN254AT"         BN254AT._h         BN254AT._q         BN254AT._r         BN254AT.gA         BN254AT.gJ         BN254AT.gP
  , testWeierstrass' 100          "BN254B"          BN254B._h          BN254B._q          BN254B._r          BN254B.gA          BN254B.gJ          BN254B.gP
  , testWeierstrass'  10         "BN254BT"         BN254BT._h         BN254BT._q         BN254BT._r         BN254BT.gA         BN254BT.gJ         BN254BT.gP
  , testWeierstrass' 100           "BN256"           BN256._h           BN256._q           BN256._r           BN256.gA           BN256.gJ           BN256.gP
  , testWeierstrass' 100           "BN384"           BN384._h           BN384._q           BN384._r           BN384.gA           BN384.gJ           BN384.gP
  , testWeierstrass' 100           "BN462"           BN462._h           BN462._q           BN462._r           BN462.gA           BN462.gJ           BN462.gP
  , testWeierstrass'  10          "BN462T"          BN462T._h          BN462T._q          BN462T._r          BN462T.gA          BN462T.gJ          BN462T.gP
  , testWeierstrass' 100           "BN512"           BN512._h           BN512._q           BN512._r           BN512.gA           BN512.gJ           BN512.gP
  , testWeierstrass' 100 "BrainpoolP160R1" BrainpoolP160R1._h BrainpoolP160R1._q BrainpoolP160R1._r BrainpoolP160R1.gA BrainpoolP160R1.gJ BrainpoolP160R1.gP
  , testWeierstrass' 100 "BrainpoolP160T1" BrainpoolP160T1._h BrainpoolP160T1._q BrainpoolP160T1._r BrainpoolP160T1.gA BrainpoolP160T1.gJ BrainpoolP160T1.gP
  , testWeierstrass' 100 "BrainpoolP192R1" BrainpoolP192R1._h BrainpoolP192R1._q BrainpoolP192R1._r BrainpoolP192R1.gA BrainpoolP192R1.gJ BrainpoolP192R1.gP
  , testWeierstrass' 100 "BrainpoolP192T1" BrainpoolP192T1._h BrainpoolP192T1._q BrainpoolP192T1._r BrainpoolP192T1.gA BrainpoolP192T1.gJ BrainpoolP192T1.gP
  , testWeierstrass' 100 "BrainpoolP224R1" BrainpoolP224R1._h BrainpoolP224R1._q BrainpoolP224R1._r BrainpoolP224R1.gA BrainpoolP224R1.gJ BrainpoolP224R1.gP
  , testWeierstrass' 100 "BrainpoolP224T1" BrainpoolP224T1._h BrainpoolP224T1._q BrainpoolP224T1._r BrainpoolP224T1.gA BrainpoolP224T1.gJ BrainpoolP224T1.gP
  , testWeierstrass' 100 "BrainpoolP256R1" BrainpoolP256R1._h BrainpoolP256R1._q BrainpoolP256R1._r BrainpoolP256R1.gA BrainpoolP256R1.gJ BrainpoolP256R1.gP
  , testWeierstrass' 100 "BrainpoolP256T1" BrainpoolP256T1._h BrainpoolP256T1._q BrainpoolP256T1._r BrainpoolP256T1.gA BrainpoolP256T1.gJ BrainpoolP256T1.gP
  , testWeierstrass' 100 "BrainpoolP320R1" BrainpoolP320R1._h BrainpoolP320R1._q BrainpoolP320R1._r BrainpoolP320R1.gA BrainpoolP320R1.gJ BrainpoolP320R1.gP
  , testWeierstrass' 100 "BrainpoolP320T1" BrainpoolP320T1._h BrainpoolP320T1._q BrainpoolP320T1._r BrainpoolP320T1.gA BrainpoolP320T1.gJ BrainpoolP320T1.gP
  , testWeierstrass' 100 "BrainpoolP384R1" BrainpoolP384R1._h BrainpoolP384R1._q BrainpoolP384R1._r BrainpoolP384R1.gA BrainpoolP384R1.gJ BrainpoolP384R1.gP
  , testWeierstrass' 100 "BrainpoolP384T1" BrainpoolP384T1._h BrainpoolP384T1._q BrainpoolP384T1._r BrainpoolP384T1.gA BrainpoolP384T1.gJ BrainpoolP384T1.gP
  , testWeierstrass' 100 "BrainpoolP512R1" BrainpoolP512R1._h BrainpoolP512R1._q BrainpoolP512R1._r BrainpoolP512R1.gA BrainpoolP512R1.gJ BrainpoolP512R1.gP
  , testWeierstrass' 100 "BrainpoolP512T1" BrainpoolP512T1._h BrainpoolP512T1._q BrainpoolP512T1._r BrainpoolP512T1.gA BrainpoolP512T1.gJ BrainpoolP512T1.gP
  , testWeierstrass' 100       "SECP112R1"       SECP112R1._h       SECP112R1._q       SECP112R1._r       SECP112R1.gA       SECP112R1.gJ       SECP112R1.gP
  , testWeierstrass' 100       "SECP112R2"       SECP112R2._h       SECP112R2._q       SECP112R2._r       SECP112R2.gA       SECP112R2.gJ       SECP112R2.gP
  , testWeierstrass' 100       "SECP128R1"       SECP128R1._h       SECP128R1._q       SECP128R1._r       SECP128R1.gA       SECP128R1.gJ       SECP128R1.gP
  , testWeierstrass' 100       "SECP128R2"       SECP128R2._h       SECP128R2._q       SECP128R2._r       SECP128R2.gA       SECP128R2.gJ       SECP128R2.gP
  , testWeierstrass' 100       "SECP160K1"       SECP160K1._h       SECP160K1._q       SECP160K1._r       SECP160K1.gA       SECP160K1.gJ       SECP160K1.gP
  , testWeierstrass' 100       "SECP160R1"       SECP160R1._h       SECP160R1._q       SECP160R1._r       SECP160R1.gA       SECP160R1.gJ       SECP160R1.gP
  , testWeierstrass' 100       "SECP160R2"       SECP160R2._h       SECP160R2._q       SECP160R2._r       SECP160R2.gA       SECP160R2.gJ       SECP160R2.gP
  , testWeierstrass' 100       "SECP192K1"       SECP192K1._h       SECP192K1._q       SECP192K1._r       SECP192K1.gA       SECP192K1.gJ       SECP192K1.gP
  , testWeierstrass' 100       "SECP192R1"       SECP192R1._h       SECP192R1._q       SECP192R1._r       SECP192R1.gA       SECP192R1.gJ       SECP192R1.gP
  , testWeierstrass' 100       "SECP224K1"       SECP224K1._h       SECP224K1._q       SECP224K1._r       SECP224K1.gA       SECP224K1.gJ       SECP224K1.gP
  , testWeierstrass' 100       "SECP224R1"       SECP224R1._h       SECP224R1._q       SECP224R1._r       SECP224R1.gA       SECP224R1.gJ       SECP224R1.gP
  , testWeierstrass' 100       "SECP256K1"       SECP256K1._h       SECP256K1._q       SECP256K1._r       SECP256K1.gA       SECP256K1.gJ       SECP256K1.gP
  , testWeierstrass' 100       "SECP256R1"       SECP256R1._h       SECP256R1._q       SECP256R1._r       SECP256R1.gA       SECP256R1.gJ       SECP256R1.gP
  , testWeierstrass' 100       "SECP384R1"       SECP384R1._h       SECP384R1._q       SECP384R1._r       SECP384R1.gA       SECP384R1.gJ       SECP384R1.gP
  , testWeierstrass' 100       "SECP521R1"       SECP521R1._h       SECP521R1._q       SECP521R1._r       SECP521R1.gA       SECP521R1.gJ       SECP521R1.gP
  ]
  where
    testWeierstrass' n c h q r a j p = localOption (QuickCheckTests n) $ testGroup c
      [ test "Affine" a h q r
      , test "Jacobian" j h q r
      , test "Projective" p h q r
      ]
