module Main where

import Protolude

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
import GaloisField
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

identities :: Eq a => (a -> a -> a) -> a -> a -> Bool
identities op e x = op x e == x && op e x == x

inverses :: Eq a => (a -> a -> a) -> (a -> a) -> a -> a -> Bool
inverses op neg e x = op x (neg x) == e && op (neg x) x == e

commutativity :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutativity op x y = op x y == op y x

associativity :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associativity op x y z = op x (op y z) == op (op x y) z

groupAxioms :: forall r c k .
  (Arbitrary (Point r c k), Curve r c k, Eq (Point r c k), GaloisField k, Show (Point r c k))
  => Point r c k -> TestTree
groupAxioms _ = testGroup "Group axioms"
  [ testCase "identity closure" $
    def (id :: Point r c k) @?= True
  , testProperty "point closure" $
    def . (identity :: Point r c k -> Point r c k)
  , testProperty "inversion closure" $
    def . (inv :: Point r c k -> Point r c k)
  , testProperty "addition closure" $
    (.) def . (add :: Point r c k -> Point r c k -> Point r c k)
  , testProperty "doubling closure" $
    def . (double :: Point r c k -> Point r c k)
  , testProperty "multiplication closure" $
    def . (mul 6 :: Point r c k -> Point r c k)
  , testProperty "identity" $
    identities (add :: Point r c k -> Point r c k -> Point r c k) mempty
  , testProperty "inverses" $
    inverses (add :: Point r c k -> Point r c k -> Point r c k) inv mempty
  , testProperty "commutativity" $
    commutativity (add :: Point r c k -> Point r c k -> Point r c k)
  , testProperty "associativity" $
    associativity (add :: Point r c k -> Point r c k -> Point r c k)
  ]

curveParameters :: forall r c k .
  (Arbitrary (Point r c k), Curve r c k, Eq (Point r c k), GaloisField k, Show (Point r c k))
  => k -> k -> Point r c k -> Integer -> Integer -> Integer -> TestTree
curveParameters a b g h n p = testGroup "Curve parameters"
  [ testCase "field characteristic" $
    char (witness :: k) @?= p
  ]

test :: forall r c k .
  (Arbitrary (Point r c k), Curve r c k, Eq (Point r c k), GaloisField k, Show (Point r c k))
  => TestName -> k -> k -> Point r c k -> Integer -> Integer -> Integer -> TestTree
test s a b g h n p = testGroup s [groupAxioms g, curveParameters a b g h n p]

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Binary"
    [ test        "SECT113R1"       SECT113R1._a       SECT113R1._b       SECT113R1._g       SECT113R1._h       SECT113R1._n                  2
    , test        "SECT113R2"       SECT113R2._a       SECT113R2._b       SECT113R2._g       SECT113R2._h       SECT113R2._n                  2
    , test        "SECT131R1"       SECT131R1._a       SECT131R1._b       SECT131R1._g       SECT131R1._h       SECT131R1._n                  2
    , test        "SECT131R2"       SECT131R2._a       SECT131R2._b       SECT131R2._g       SECT131R2._h       SECT131R2._n                  2
    , test        "SECT163K1"       SECT163K1._a       SECT163K1._b       SECT163K1._g       SECT163K1._h       SECT163K1._n                  2
    , test        "SECT163R1"       SECT163R1._a       SECT163R1._b       SECT163R1._g       SECT163R1._h       SECT163R1._n                  2
    , test        "SECT163R2"       SECT163R2._a       SECT163R2._b       SECT163R2._g       SECT163R2._h       SECT163R2._n                  2
    , test        "SECT193R1"       SECT193R1._a       SECT193R1._b       SECT193R1._g       SECT193R1._h       SECT193R1._n                  2
    , test        "SECT193R2"       SECT193R2._a       SECT193R2._b       SECT193R2._g       SECT193R2._h       SECT193R2._n                  2
    , test        "SECT233K1"       SECT233K1._a       SECT233K1._b       SECT233K1._g       SECT233K1._h       SECT233K1._n                  2
    , test        "SECT233R1"       SECT233R1._a       SECT233R1._b       SECT233R1._g       SECT233R1._h       SECT233R1._n                  2
    , test        "SECT239K1"       SECT239K1._a       SECT239K1._b       SECT239K1._g       SECT239K1._h       SECT239K1._n                  2
    , test        "SECT283K1"       SECT283K1._a       SECT283K1._b       SECT283K1._g       SECT283K1._h       SECT283K1._n                  2
    , test        "SECT283R1"       SECT283R1._a       SECT283R1._b       SECT283R1._g       SECT283R1._h       SECT283R1._n                  2
    , test        "SECT409K1"       SECT409K1._a       SECT409K1._b       SECT409K1._g       SECT409K1._h       SECT409K1._n                  2
    , test        "SECT409R1"       SECT409R1._a       SECT409R1._b       SECT409R1._g       SECT409R1._h       SECT409R1._n                  2
    , test        "SECT571K1"       SECT571K1._a       SECT571K1._b       SECT571K1._g       SECT571K1._h       SECT571K1._n                  2
    , test        "SECT571R1"       SECT571R1._a       SECT571R1._b       SECT571R1._g       SECT571R1._h       SECT571R1._n                  2
    ]                                                                                                                                          
  , testGroup "Edwards"                                                                                                                        
    [ test        "Curve1174"       Curve1174._a       Curve1174._d       Curve1174._g       Curve1174._h       Curve1174._n       Curve1174._p
    , test       "Curve41417"      Curve41417._a      Curve41417._d      Curve41417._g      Curve41417._h      Curve41417._n      Curve41417._p
    , test            "E-222"            E222._a            E222._d            E222._g            E222._h            E222._n            E222._p
    , test            "E-382"            E382._a            E382._d            E382._g            E382._h            E382._n            E382._p
    , test            "E-521"            E521._a            E521._d            E521._g            E521._h            E521._n            E521._p
    , test            "Ed448"           Ed448._a           Ed448._d           Ed448._g           Ed448._h           Ed448._n           Ed448._p
    , test           "Ed3363"          Ed3363._a          Ed3363._d          Ed3363._g          Ed3363._h          Ed3363._n          Ed3363._p
    , test          "Ed25519"         Ed25519._a         Ed25519._d         Ed25519._g         Ed25519._h         Ed25519._n         Ed25519._p
    , test           "JubJub"          JubJub._a          JubJub._d          JubJub._g          JubJub._h          JubJub._n          JubJub._p
    ]                                                                                                                                          
  , testGroup "Montgomery"                                                                                                                     
    [ test         "Curve448"        Curve448._a        Curve448._b        Curve448._g        Curve448._h        Curve448._n        Curve448._p
    , test       "Curve25519"      Curve25519._a      Curve25519._b      Curve25519._g      Curve25519._h      Curve25519._n      Curve25519._p
    , test      "Curve383187"     Curve383187._a     Curve383187._b     Curve383187._g     Curve383187._h     Curve383187._n     Curve383187._p
    , test            "M-221"            M221._a            M221._b            M221._g            M221._h            M221._n            M221._p
    , test            "M-383"            M383._a            M383._b            M383._g            M383._h            M383._n            M383._p
    , test            "M-511"            M511._a            M511._b            M511._g            M511._h            M511._n            M511._p
    ]                                                                                                                                          
  , testGroup "Weierstrass"                                                                                                                    
    [ test   "ANSSI-FRP256V1"   ANSSIFRP256V1._a   ANSSIFRP256V1._b   ANSSIFRP256V1._g   ANSSIFRP256V1._h   ANSSIFRP256V1._n   ANSSIFRP256V1._p
    , test        "BLS12-381"       BLS12_381._a       BLS12_381._b       BLS12_381._g       BLS12_381._h       BLS12_381._n       BLS12_381._p
    , test       "BLS12-381T"      BLS12_381T._a      BLS12_381T._b      BLS12_381T._g      BLS12_381T._h      BLS12_381T._n      BLS12_381T._p
    , test        "BLS48-581"       BLS48_581._a       BLS48_581._b       BLS48_581._g       BLS48_581._h       BLS48_581._n       BLS48_581._p
    , test       "BLS48-581T"      BLS48_581T._a      BLS48_581T._b      BLS48_581T._g      BLS48_581T._h      BLS48_581T._n      BLS48_581T._p
    , test            "BN224"           BN224._a           BN224._b           BN224._g           BN224._h           BN224._n           BN224._p
    , test            "BN254"           BN254._a           BN254._b           BN254._g           BN254._h           BN254._n           BN254._p
    , test           "BN254T"          BN254T._a          BN254T._b          BN254T._g          BN254T._h          BN254T._n          BN254T._p
    , test           "BN254A"          BN254A._a          BN254A._b          BN254A._g          BN254A._h          BN254A._n          BN254A._p
    , test          "BN254AT"         BN254AT._a         BN254AT._b         BN254AT._g         BN254AT._h         BN254AT._n         BN254AT._p
    , test           "BN254B"          BN254B._a          BN254B._b          BN254B._g          BN254B._h          BN254B._n          BN254B._p
    , test          "BN254BT"         BN254BT._a         BN254BT._b         BN254BT._g         BN254BT._h         BN254BT._n         BN254BT._p
    , test            "BN256"           BN256._a           BN256._b           BN256._g           BN256._h           BN256._n           BN256._p
    , test            "BN384"           BN384._a           BN384._b           BN384._g           BN384._h           BN384._n           BN384._p
    , test            "BN462"           BN462._a           BN462._b           BN462._g           BN462._h           BN462._n           BN462._p
    , test           "BN462T"          BN462T._a          BN462T._b          BN462T._g          BN462T._h          BN462T._n          BN462T._p
    , test            "BN512"           BN512._a           BN512._b           BN512._g           BN512._h           BN512._n           BN512._p
    , test "Brainpool-P160R1" BrainpoolP160R1._a BrainpoolP160R1._b BrainpoolP160R1._g BrainpoolP160R1._h BrainpoolP160R1._n BrainpoolP160R1._p
    , test "Brainpool-P160T1" BrainpoolP160T1._a BrainpoolP160T1._b BrainpoolP160T1._g BrainpoolP160T1._h BrainpoolP160T1._n BrainpoolP160T1._p
    , test "Brainpool-P192R1" BrainpoolP192R1._a BrainpoolP192R1._b BrainpoolP192R1._g BrainpoolP192R1._h BrainpoolP192R1._n BrainpoolP192R1._p
    , test "Brainpool-P192T1" BrainpoolP192T1._a BrainpoolP192T1._b BrainpoolP192T1._g BrainpoolP192T1._h BrainpoolP192T1._n BrainpoolP192T1._p
    , test "Brainpool-P224R1" BrainpoolP224R1._a BrainpoolP224R1._b BrainpoolP224R1._g BrainpoolP224R1._h BrainpoolP224R1._n BrainpoolP224R1._p
    , test "Brainpool-P224T1" BrainpoolP224T1._a BrainpoolP224T1._b BrainpoolP224T1._g BrainpoolP224T1._h BrainpoolP224T1._n BrainpoolP224T1._p
    , test "Brainpool-P256R1" BrainpoolP256R1._a BrainpoolP256R1._b BrainpoolP256R1._g BrainpoolP256R1._h BrainpoolP256R1._n BrainpoolP256R1._p
    , test "Brainpool-P256T1" BrainpoolP256T1._a BrainpoolP256T1._b BrainpoolP256T1._g BrainpoolP256T1._h BrainpoolP256T1._n BrainpoolP256T1._p
    , test "Brainpool-P320R1" BrainpoolP320R1._a BrainpoolP320R1._b BrainpoolP320R1._g BrainpoolP320R1._h BrainpoolP320R1._n BrainpoolP320R1._p
    , test "Brainpool-P320T1" BrainpoolP320T1._a BrainpoolP320T1._b BrainpoolP320T1._g BrainpoolP320T1._h BrainpoolP320T1._n BrainpoolP320T1._p
    , test "Brainpool-P384R1" BrainpoolP384R1._a BrainpoolP384R1._b BrainpoolP384R1._g BrainpoolP384R1._h BrainpoolP384R1._n BrainpoolP384R1._p
    , test "Brainpool-P384T1" BrainpoolP384T1._a BrainpoolP384T1._b BrainpoolP384T1._g BrainpoolP384T1._h BrainpoolP384T1._n BrainpoolP384T1._p
    , test "Brainpool-P512R1" BrainpoolP512R1._a BrainpoolP512R1._b BrainpoolP512R1._g BrainpoolP512R1._h BrainpoolP512R1._n BrainpoolP512R1._p
    , test "Brainpool-P512T1" BrainpoolP512T1._a BrainpoolP512T1._b BrainpoolP512T1._g BrainpoolP512T1._h BrainpoolP512T1._n BrainpoolP512T1._p
    , test        "SECP112R1"       SECP112R1._a       SECP112R1._b       SECP112R1._g       SECP112R1._h       SECP112R1._n       SECP112R1._p
    , test        "SECP112R2"       SECP112R2._a       SECP112R2._b       SECP112R2._g       SECP112R2._h       SECP112R2._n       SECP112R2._p
    , test        "SECP128R1"       SECP128R1._a       SECP128R1._b       SECP128R1._g       SECP128R1._h       SECP128R1._n       SECP128R1._p
    , test        "SECP128R2"       SECP128R2._a       SECP128R2._b       SECP128R2._g       SECP128R2._h       SECP128R2._n       SECP128R2._p
    , test        "SECP160K1"       SECP160K1._a       SECP160K1._b       SECP160K1._g       SECP160K1._h       SECP160K1._n       SECP160K1._p
    , test        "SECP160R1"       SECP160R1._a       SECP160R1._b       SECP160R1._g       SECP160R1._h       SECP160R1._n       SECP160R1._p
    , test        "SECP160R2"       SECP160R2._a       SECP160R2._b       SECP160R2._g       SECP160R2._h       SECP160R2._n       SECP160R2._p
    , test        "SECP192K1"       SECP192K1._a       SECP192K1._b       SECP192K1._g       SECP192K1._h       SECP192K1._n       SECP192K1._p
    , test        "SECP192R1"       SECP192R1._a       SECP192R1._b       SECP192R1._g       SECP192R1._h       SECP192R1._n       SECP192R1._p
    , test        "SECP224K1"       SECP224K1._a       SECP224K1._b       SECP224K1._g       SECP224K1._h       SECP224K1._n       SECP224K1._p
    , test        "SECP224R1"       SECP224R1._a       SECP224R1._b       SECP224R1._g       SECP224R1._h       SECP224R1._n       SECP224R1._p
    , test        "SECP256K1"       SECP256K1._a       SECP256K1._b       SECP256K1._g       SECP256K1._h       SECP256K1._n       SECP256K1._p
    , test        "SECP256R1"       SECP256R1._a       SECP256R1._b       SECP256R1._g       SECP256R1._h       SECP256R1._n       SECP256R1._p
    , test        "SECP384R1"       SECP384R1._a       SECP384R1._b       SECP384R1._g       SECP384R1._h       SECP384R1._n       SECP384R1._p
    , test        "SECP521R1"       SECP521R1._a       SECP521R1._b       SECP521R1._g       SECP521R1._h       SECP521R1._n       SECP521R1._p
    ]
  ]
