module CurveTests where

import Protolude

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
import qualified Curve.ShortWeierstrass.BN224 as BN224
import qualified Curve.ShortWeierstrass.BN254A as BN254A
import qualified Curve.ShortWeierstrass.BN254A2 as BN254A2
import qualified Curve.ShortWeierstrass.BN254B as BN254B
import qualified Curve.ShortWeierstrass.BN254B2 as BN254B2
import qualified Curve.ShortWeierstrass.BN254P as BN254P
import qualified Curve.ShortWeierstrass.BN254P2 as BN254P2
import qualified Curve.ShortWeierstrass.BN256 as BN256
import qualified Curve.ShortWeierstrass.BN384 as BN384
import qualified Curve.ShortWeierstrass.BN512 as BN512
import qualified Curve.ShortWeierstrass.BrainpoolP160R1 as BrainpoolP160R1
import qualified Curve.ShortWeierstrass.BrainpoolP160T1 as BrainpoolP160T1
import qualified Curve.ShortWeierstrass.BrainpoolP192R1 as BrainpoolP192R1
import qualified Curve.ShortWeierstrass.BrainpoolP192T1 as BrainpoolP192T1
import qualified Curve.ShortWeierstrass.BrainpoolP224R1 as BrainpoolP224R1
import qualified Curve.ShortWeierstrass.BrainpoolP224T1 as BrainpoolP224T1
import qualified Curve.ShortWeierstrass.BrainpoolP256R1 as BrainpoolP256R1
import qualified Curve.ShortWeierstrass.BrainpoolP256T1 as BrainpoolP256T1
import qualified Curve.ShortWeierstrass.BrainpoolP320R1 as BrainpoolP320R1
import qualified Curve.ShortWeierstrass.BrainpoolP320T1 as BrainpoolP320T1
import qualified Curve.ShortWeierstrass.BrainpoolP384R1 as BrainpoolP384R1
import qualified Curve.ShortWeierstrass.BrainpoolP384T1 as BrainpoolP384T1
import qualified Curve.ShortWeierstrass.BrainpoolP512R1 as BrainpoolP512R1
import qualified Curve.ShortWeierstrass.BrainpoolP512T1 as BrainpoolP512T1
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
  => Point r c k -> TestName -> TestTree
groupAxioms _ str = testGroup ("Test group axioms of " <> str)
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

test_sect113r1 :: TestTree
test_sect113r1 = groupAxioms (witness :: SECT113R1.P) "SECT113R1"

test_sect113r2 :: TestTree
test_sect113r2 = groupAxioms (witness :: SECT113R2.P) "SECT113R2"

test_sect131r1 :: TestTree
test_sect131r1 = groupAxioms (witness :: SECT131R1.P) "SECT131R1"

test_sect131r2 :: TestTree
test_sect131r2 = groupAxioms (witness :: SECT131R2.P) "SECT131R2"

test_sect163k1 :: TestTree
test_sect163k1 = groupAxioms (witness :: SECT163K1.P) "SECT163K1"

test_sect163r1 :: TestTree
test_sect163r1 = groupAxioms (witness :: SECT163R1.P) "SECT163R1"

test_sect163r2 :: TestTree
test_sect163r2 = groupAxioms (witness :: SECT163R2.P) "SECT163R2"

test_sect193r1 :: TestTree
test_sect193r1 = groupAxioms (witness :: SECT193R1.P) "SECT193R1"

test_sect193r2 :: TestTree
test_sect193r2 = groupAxioms (witness :: SECT193R2.P) "SECT193R2"

test_sect233k1 :: TestTree
test_sect233k1 = groupAxioms (witness :: SECT233K1.P) "SECT233K1"

test_sect233r1 :: TestTree
test_sect233r1 = groupAxioms (witness :: SECT233R1.P) "SECT233R1"

test_sect239k1 :: TestTree
test_sect239k1 = groupAxioms (witness :: SECT239K1.P) "SECT239K1"

test_sect283k1 :: TestTree
test_sect283k1 = groupAxioms (witness :: SECT283K1.P) "SECT283K1"

test_sect283r1 :: TestTree
test_sect283r1 = groupAxioms (witness :: SECT283R1.P) "SECT283R1"

test_sect409k1 :: TestTree
test_sect409k1 = groupAxioms (witness :: SECT409K1.P) "SECT409K1"

test_sect409r1 :: TestTree
test_sect409r1 = groupAxioms (witness :: SECT409R1.P) "SECT409R1"

test_sect571k1 :: TestTree
test_sect571k1 = groupAxioms (witness :: SECT571K1.P) "SECT571K1"

test_sect571r1 :: TestTree
test_sect571r1 = groupAxioms (witness :: SECT571R1.P) "SECT571R1"

test_curve448 :: TestTree
test_curve448 = groupAxioms (witness :: Curve448.P) "Curve448"

test_curve25519 :: TestTree
test_curve25519 = groupAxioms (witness :: Curve25519.P) "Curve25519"

test_curve383187 :: TestTree
test_curve383187 = groupAxioms (witness :: Curve383187.P) "Curve383187"

test_m221 :: TestTree
test_m221 = groupAxioms (witness :: M221.P) "M-221"

test_m383 :: TestTree
test_m383 = groupAxioms (witness :: M383.P) "M-383"

test_m511 :: TestTree
test_m511 = groupAxioms (witness :: M511.P) "M-511"

test_anssi_frp256v1 :: TestTree
test_anssi_frp256v1 = groupAxioms (witness :: ANSSIFRP256V1.P) "ANSSI-FRP256V1"

test_bls12_381_g1 :: TestTree
test_bls12_381_g1 = groupAxioms (witness :: BLS12_381.G1.P) "BLS12_381.G1"

test_bls12_381_g2 :: TestTree
test_bls12_381_g2 = groupAxioms (witness :: BLS12_381.G2.P) "BLS12_381.G2"

test_bn224 :: TestTree
test_bn224 = groupAxioms (witness :: BN224.P) "BN224"

test_bn254a :: TestTree
test_bn254a = groupAxioms (witness :: BN254A.P) "BN254A"

test_bn254a2 :: TestTree
test_bn254a2 = groupAxioms (witness :: BN254A2.P) "BN254A2"

test_bn254b :: TestTree
test_bn254b = groupAxioms (witness :: BN254B.P) "BN254B"

test_bn254b2 :: TestTree
test_bn254b2 = groupAxioms (witness :: BN254B2.P) "BN254B2"

test_bn254p :: TestTree
test_bn254p = groupAxioms (witness :: BN254P.P) "BN254P"

test_bn254p2 :: TestTree
test_bn254p2 = groupAxioms (witness :: BN254P2.P) "BN254P2"

test_bn256 :: TestTree
test_bn256 = groupAxioms (witness :: BN256.P) "BN256"

test_bn384 :: TestTree
test_bn384 = groupAxioms (witness :: BN384.P) "BN384"

test_bn512 :: TestTree
test_bn512 = groupAxioms (witness :: BN512.P) "BN512"

test_brainpool_p160r1 :: TestTree
test_brainpool_p160r1 = groupAxioms (witness :: BrainpoolP160R1.P) "Brainpool-P160R1"

test_brainpool_p160t1 :: TestTree
test_brainpool_p160t1 = groupAxioms (witness :: BrainpoolP160T1.P) "Brainpool-P160T1"

test_brainpool_p192r1 :: TestTree
test_brainpool_p192r1 = groupAxioms (witness :: BrainpoolP192R1.P) "Brainpool-P192R1"

test_brainpool_p192t1 :: TestTree
test_brainpool_p192t1 = groupAxioms (witness :: BrainpoolP192T1.P) "Brainpool-P192T1"

test_brainpool_p224r1 :: TestTree
test_brainpool_p224r1 = groupAxioms (witness :: BrainpoolP224R1.P) "Brainpool-P224R1"

test_brainpool_p224t1 :: TestTree
test_brainpool_p224t1 = groupAxioms (witness :: BrainpoolP224T1.P) "Brainpool-P224T1"

test_brainpool_p256r1 :: TestTree
test_brainpool_p256r1 = groupAxioms (witness :: BrainpoolP256R1.P) "Brainpool-P256R1"

test_brainpool_p256t1 :: TestTree
test_brainpool_p256t1 = groupAxioms (witness :: BrainpoolP256T1.P) "Brainpool-P256T1"

test_brainpool_p320r1 :: TestTree
test_brainpool_p320r1 = groupAxioms (witness :: BrainpoolP320R1.P) "Brainpool-P320R1"

test_brainpool_p320t1 :: TestTree
test_brainpool_p320t1 = groupAxioms (witness :: BrainpoolP320T1.P) "Brainpool-P320T1"

test_brainpool_p384r1 :: TestTree
test_brainpool_p384r1 = groupAxioms (witness :: BrainpoolP384R1.P) "Brainpool-P384R1"

test_brainpool_p384t1 :: TestTree
test_brainpool_p384t1 = groupAxioms (witness :: BrainpoolP384T1.P) "Brainpool-P384T1"

test_brainpool_p512r1 :: TestTree
test_brainpool_p512r1 = groupAxioms (witness :: BrainpoolP512R1.P) "Brainpool-P512R1"

test_brainpool_p512t1 :: TestTree
test_brainpool_p512t1 = groupAxioms (witness :: BrainpoolP512T1.P) "Brainpool-P512T1"

test_secp112r1 :: TestTree
test_secp112r1 = groupAxioms (witness :: SECP112R1.P) "SECP112R1"

test_secp112r2 :: TestTree
test_secp112r2 = groupAxioms (witness :: SECP112R2.P) "SECP112R2"

test_secp128r1 :: TestTree
test_secp128r1 = groupAxioms (witness :: SECP128R1.P) "SECP128R1"

test_secp128r2 :: TestTree
test_secp128r2 = groupAxioms (witness :: SECP128R2.P) "SECP128R2"

test_secp160k1 :: TestTree
test_secp160k1 = groupAxioms (witness :: SECP160K1.P) "SECP160K1"

test_secp160r1 :: TestTree
test_secp160r1 = groupAxioms (witness :: SECP160R1.P) "SECP160R1"

test_secp160r2 :: TestTree
test_secp160r2 = groupAxioms (witness :: SECP160R2.P) "SECP160R2"

test_secp192k1 :: TestTree
test_secp192k1 = groupAxioms (witness :: SECP192K1.P) "SECP192K1"

test_secp192r1 :: TestTree
test_secp192r1 = groupAxioms (witness :: SECP192R1.P) "SECP192R1"

test_secp224k1 :: TestTree
test_secp224k1 = groupAxioms (witness :: SECP224K1.P) "SECP224K1"

test_secp224r1 :: TestTree
test_secp224r1 = groupAxioms (witness :: SECP224R1.P) "SECP224R1"

test_secp256k1 :: TestTree
test_secp256k1 = groupAxioms (witness :: SECP256K1.P) "SECP256K1"

test_secp256r1 :: TestTree
test_secp256r1 = groupAxioms (witness :: SECP256R1.P) "SECP256R1"

test_secp384r1 :: TestTree
test_secp384r1 = groupAxioms (witness :: SECP384R1.P) "SECP384R1"

test_secp521r1 :: TestTree
test_secp521r1 = groupAxioms (witness :: SECP521R1.P) "SECP521R1"

test_curve1174 :: TestTree
test_curve1174 = groupAxioms (witness :: Curve1174.P) "Curve1174"

test_curve41417 :: TestTree
test_curve41417 = groupAxioms (witness :: Curve41417.P) "Curve41417"

test_e222 :: TestTree
test_e222 = groupAxioms (witness :: E222.P) "E-222"

test_e382 :: TestTree
test_e382 = groupAxioms (witness :: E382.P) "E-382"

test_e521 :: TestTree
test_e521 = groupAxioms (witness :: E521.P) "E-521"

test_ed448 :: TestTree
test_ed448 = groupAxioms (witness :: Ed448.P) "Ed448"

test_ed3363 :: TestTree
test_ed3363 = groupAxioms (witness :: Ed3363.P) "Ed3363"

test_ed25519 :: TestTree
test_ed25519 = groupAxioms (witness :: Ed25519.P) "Ed25519"

test_jubjub :: TestTree
test_jubjub = groupAxioms (witness :: JubJub.P) "JubJub"
