module CurveTests where

import Protolude

import Curve
import Curve.Field
import GaloisField
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
    def . (double :: g -> g)
  , testProperty "multiplication closure" $
    def . (flip mul 3 :: g -> g)
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
hasse h n q = (h * n - q - 1) ^ (2 :: Int) <= 4 * q

curveParameters :: forall r c k . (Curve r c k, GaloisField k,
  Arbitrary (Point r c k), Eq (Point r c k), Show (Point r c k))
  => Point r c k -> Integer -> Integer -> Integer -> TestTree
curveParameters g h n p = testGroup "Curve parameters"
  [ testCase "generator is parametrised" $
    gen @?= g
  , testCase "cofactor is parametrised" $
    cof (witness :: Point r c k) @?= h
  , testCase "order is parametrised" $
    Curve.order (witness :: Point r c k) @?= n
  , testCase "characteristic is parametrised" $
    char (witness :: k) @?= p
  , testCase "characteristic is prime" $
    isPrime p @?= True
  , testCase "discriminant is nonzero" $
    disc (witness :: Point r c k) /= 0 @?= True
  , testCase "generator is well-defined" $
    def (gen :: Point r c k) @?= True
  , testCase "generator is in cyclic subgroup" $
    mul g n @?= mempty
  , testCase "cyclic subgroup has prime order" $
    isPrime n @?= True
  , testCase "hasse theorem holds" $
    hasse h n (GaloisField.order (witness :: k)) @?= True
  ]

test :: forall r c k . (Curve r c k, GaloisField k,
  Arbitrary (Point r c k), Eq (Point r c k), Show (Point r c k))
  => TestName -> Point r c k -> Integer -> Integer -> Integer -> TestTree
test s g h n p = testGroup s [groupAxioms g, curveParameters g h n p]

groupParameters :: forall k . (FGroup k, GaloisField k)
  => Element k -> Integer -> Integer -> TestTree
groupParameters g n p = testGroup "Group parameters"
  [ testCase "generator is parametrised" $
    gen @?= g
  , testCase "order is parametrised" $
    Curve.order (witness :: Element k) @?= n
  , testCase "characteristic is parametrised" $
    char (witness :: k) @?= p
  , testCase "characteristic is prime" $
    isPrime p @?= True
  , testCase "generator is well-defined" $
    def (gen :: Element k) @?= True
  , testCase "generator is in cyclic subgroup" $
    mul g n @?= mempty
  ]

test' :: forall k . (FGroup k, GaloisField k)
  => TestName -> Element k -> Integer -> Integer -> TestTree
test' s g n p = testGroup s [groupAxioms g, groupParameters g n p]
