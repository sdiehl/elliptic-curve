module GroupTests where

import Protolude

import Curve
import GaloisField
import Group
import Group.Field
import Math.NumberTheory.Primes.Testing
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

groupAxioms :: forall g . Group g => g -> TestTree
groupAxioms _ = testGroup "Group axioms"
  [ testCase "identity closure" $
    def (mempty :: g) @?= True
  , testProperty "point closure" $
    def . (identity :: g -> g)
  , testProperty "inversion closure" $
    def . (inv :: g -> g)
  , testProperty "addition closure" $
    (.) def . ((<>) :: g -> g -> g)
  , testProperty "doubling closure" $
    def . (join (<>) :: g -> g)
  , testProperty "multiplication closure" $
    def . (flip mul' 3 :: g -> g)
  , testProperty "identity" $
    identities ((<>) :: g -> g -> g) mempty
  , testProperty "inverses" $
    inverses ((<>) :: g -> g -> g) inv mempty
  , testProperty "commutativity" $
    commutativity ((<>) :: g -> g -> g)
  , testProperty "associativity" $
    associativity ((<>) :: g -> g -> g)
  ]

hasse :: Integer -> Integer -> Integer -> Bool
hasse h r q' = (h * r - q' - 1) ^ (2 :: Int) <= 4 * q'

curveParameters :: forall f c e q r . Curve f c e q r
  => Point f c e q r -> Integer -> Integer -> Integer -> TestTree
curveParameters g h q r = testGroup "Curve parameters"
  [ testCase "generator is parametrised" $
    gen @?= g
  , testCase "cofactor is parametrised" $
    cof (witness :: Point f c e q r) @?= h
  , testCase "characteristic is parametrised" $
    Curve.char (witness :: Point f c e q r) @?= q
  , testCase "order is parametrised" $
    Group.order (witness :: Point f c e q r) @?= r
  , testCase "characteristic is prime" $
    isPrime q @?= True
  , testCase "discriminant is nonzero" $
    disc (witness :: Point f c e q r) /= 0 @?= True
  , testCase "generator is well-defined" $
    def (gen :: Point f c e q r) @?= True
  , testCase "generator is in cyclic subgroup" $
    mul' g r @?= mempty
  , testCase "cyclic subgroup has prime order" $
    isPrime r @?= True
  , testCase "hasse theorem holds" $
    hasse h r (GaloisField.order (witness :: q)) @?= True
  ]

test :: Curve f c e q r => TestName -> Point f c e q r -> Integer -> Integer -> Integer -> TestTree
test s g h q r = testGroup s [groupAxioms g, curveParameters g h q r]

fieldParameters :: forall k . FGroup k
  => Element k -> Integer -> Integer -> TestTree
fieldParameters g q r = testGroup "Group parameters"
  [ testCase "generator is parametrised" $
    gen @?= g
  , testCase "characteristic is parametrised" $
    GaloisField.char (witness :: k) @?= q
  , testCase "order is parametrised" $
    Group.order (witness :: Element k) @?= r
  , testCase "characteristic is prime" $
    isPrime q @?= True
  , testCase "generator is well-defined" $
    def (gen :: Element k) @?= True
  , testCase "generator is in cyclic subgroup" $
    mul' g r @?= mempty
  ]

test' :: FGroup k => TestName -> Element k -> Integer -> Integer -> TestTree
test' s g q r = testGroup s [groupAxioms g, fieldParameters g q r]
