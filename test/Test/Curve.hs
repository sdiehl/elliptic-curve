module Test.Curve where

import Protolude

import Data.Curve
import qualified Data.Field.Galois as GF
import Data.Group
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

groupAxioms :: forall f c e q r . Curve f c e q r
  => Point f c e q r -> TestTree
groupAxioms _ = testGroup "Group axioms"
  [ testCase "identity closure" $
    def (mempty :: Point f c e q r) @?= True
  , testProperty "point closure" $
    (def :: Point f c e q r -> Bool)
  , testProperty "inversion closure" $
    def . (inv :: Point f c e q r -> Point f c e q r)
  , testProperty "addition closure" $
    (.) def . ((<>) :: Point f c e q r -> Point f c e q r -> Point f c e q r)
  , testProperty "doubling closure" $
    def . (join (<>) :: Point f c e q r -> Point f c e q r)
  , testProperty "multiplication closure" $
    def . (flip mul' (-3 :: Int) :: Point f c e q r -> Point f c e q r)
  , testProperty "identity" $
    identities ((<>) :: Point f c e q r -> Point f c e q r -> Point f c e q r) mempty
  , testProperty "inverses" $
    inverses ((<>) :: Point f c e q r -> Point f c e q r -> Point f c e q r) invert mempty
  , testProperty "commutativity" $
    commutativity ((<>) :: Point f c e q r -> Point f c e q r -> Point f c e q r)
  , testProperty "associativity" $
    associativity ((<>) :: Point f c e q r -> Point f c e q r -> Point f c e q r)
  ]

hasseTheorem :: Integer -> Integer -> Integer -> Bool
hasseTheorem h r q = join (*) (h * r - q - 1) <= 4 * q

doubleIdentities :: (Eq a, Eq b) => (a -> b) -> (b -> a) -> a -> b -> Bool
doubleIdentities f t e e' = f e == e' && t e' == e

doubleDefined :: (a -> Bool) -> (b -> Bool) -> (b -> a) -> b -> Bool
doubleDefined d d' t x = d' x && d (t x)

doubleInverses :: (Eq a, Eq b) => (a -> b) -> (b -> a) -> b -> Bool
doubleInverses f t x = f (t x) == x && t (f (t x)) == t x

doubleHomeomorphism :: (Eq a, Eq b) => (a -> a) -> (b -> b) -> (a -> b) -> (b -> a) -> b -> Bool
doubleHomeomorphism op op' f t x = t (op' x) == op (t x) && f (op (t x)) == op' x

curveParameters :: forall f c e q r . (Curve f 'Affine e q r, Curve f c e q r)
  => Point f c e q r -> Integer -> Integer -> Integer -> TestTree
curveParameters g h q r = testGroup "Curve parameters"
  [ testCase "generator is parametrised" $
    gen @?= g
  , testCase "cofactor is parametrised" $
    cof g @?= h
  , testCase "characteristic is parametrised" $
    char g @?= q
  , testCase "order is parametrised" $
    order g @?= r
  , testCase "characteristic is prime" $
    isPrime q @?= True
  , testCase "discriminant is nonzero" $
    disc g /= 0 @?= True
  , testCase "generator is well-defined" $
    def g @?= True
  , testCase "generator is in cyclic subgroup" $
    mul' g r @?= mempty
  , testCase "cyclic subgroup has prime order" $
    isPrime r @?= True
  , testCase "hasse theorem holds" $
    hasseTheorem h r (fromIntegral $ GF.order (witness :: q)) @?= True
  , testCase "affine transformation is doubly identity-preserving" $
    doubleIdentities fromA (toA :: Point f c e q r -> Point f 'Affine e q r) mempty mempty @?= True
  , testProperty "affine transformation is doubly well-defined" $
    doubleDefined def def (toA :: Point f c e q r -> Point f 'Affine e q r)
  , testProperty "affine transformation is doubly invertible" $
    doubleInverses fromA (toA :: Point f c e q r -> Point f 'Affine e q r)
  , testProperty "affine transformation is doubly homeomorphic" $
    doubleHomeomorphism (flip mul 3) (flip mul 3) fromA (toA :: Point f c e q r -> Point f 'Affine e q r)
  ]

test :: (Curve f c e q r, Curve f 'Affine e q r)
  => TestName -> Point f c e q r -> Integer -> Integer -> Integer -> TestTree
test s g h q r = testGroup s [groupAxioms g, curveParameters g h q r]
