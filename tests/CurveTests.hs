module CurveTests where

import Protolude

import Curve
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
    def . (flip mul (3 :: Int) :: Point r c k -> Point r c k)
  , testProperty "identity" $
    identities (add :: Point r c k -> Point r c k -> Point r c k) mempty
  , testProperty "inverses" $
    inverses (add :: Point r c k -> Point r c k -> Point r c k) inv mempty
  , testProperty "commutativity" $
    commutativity (add :: Point r c k -> Point r c k -> Point r c k)
  , testProperty "associativity" $
    associativity (add :: Point r c k -> Point r c k -> Point r c k)
  ]

hasse :: Integer -> Integer -> Integer -> Bool
hasse h n q = (h * n - q - 1) ^ (2 :: Int) <= 4 * q

curveParameters :: forall r c k .
  (Arbitrary (Point r c k), Curve r c k, Eq (Point r c k), GaloisField k, Show (Point r c k))
  => Point r c k -> Integer -> Integer -> Integer -> TestTree
curveParameters g h n p = testGroup "Curve parameters"
  [ testCase "characteristic is typed" $
    char (witness :: k) @?= p
  , testCase "characteristic is prime" $
    isPrime p @?= True
  , testCase "discriminant is nonzero" $
    disc (witness :: Point r c k) /= 0 @?= True
  , testCase "generator is in cyclic subgroup" $
    mul g n @?= id
  , testCase "cyclic subgroup has prime order" $
    isPrime n @?= True
  , testCase "hasse theorem holds" $
    hasse h n (order (witness :: k)) @?= True
  ]

test :: forall r c k .
  (Arbitrary (Point r c k), Curve r c k, Eq (Point r c k), GaloisField k, Show (Point r c k))
  => TestName -> Point r c k -> Integer -> Integer -> Integer -> TestTree
test s g h n p = testGroup s [groupAxioms g, curveParameters g h n p]
