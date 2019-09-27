module Test.Curve where

import Protolude

import Data.Curve
import qualified Data.Field.Galois as F
import Data.Group
import GHC.Natural
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Field

hasseTheorem :: Natural -> Natural -> Natural -> Bool
hasseTheorem h r q = join (*) (naturalToInteger (h * r) - naturalToInteger q - 1) <= 4 * naturalToInteger q

doubleIdentities :: (Eq a, Eq b) => (a -> b) -> (b -> a) -> a -> b -> Bool
doubleIdentities f t e e' = f e == e' && t e' == e

doubleDefined :: (a -> Bool) -> (b -> Bool) -> (b -> a) -> b -> Bool
doubleDefined d d' t x = d' x && d (t x)

doubleInverses :: (Eq a, Eq b) => (a -> b) -> (b -> a) -> b -> Bool
doubleInverses f t x = f (t x) == x && t (f (t x)) == t x

doubleHomeomorphism :: (Eq a, Eq b) => (a -> a) -> (b -> b) -> (a -> b) -> (b -> a) -> b -> Bool
doubleHomeomorphism op op' f t x = t (op' x) == op (t x) && f (op (t x)) == op' x

curveAxioms :: forall f c e q r . (Curve f 'Affine e q r, Curve f c e q r)
  => Point f c e q r -> Natural -> Natural -> Natural -> TestTree
curveAxioms g h q r = testGroup "Curve axioms"
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
  , testCase "generator is parametrised" $
    gen @?= g
  , testCase "cofactor is parametrised" $
    cof g @?= h
  , testCase "characteristic is parametrised" $
    char g @?= q
  , testCase "order is parametrised" $
    order g @?= r
  , testCase "discriminant is nonzero" $
    disc g /= 0 @?= True
  , testCase "generator is well-defined" $
    def g @?= True
  , testCase "generator is in cyclic subgroup" $
    mul' g r @?= mempty
  , testCase "hasse theorem holds" $
    hasseTheorem h r (F.order (witness :: q)) @?= True
  , testCase "affine transformation is doubly identity-preserving" $
    doubleIdentities fromA (toA :: Point f c e q r -> Point f 'Affine e q r) mempty mempty @?= True
  , testProperty "affine transformation is doubly well-defined" $
    doubleDefined def def (toA :: Point f c e q r -> Point f 'Affine e q r)
  , testProperty "affine transformation is doubly invertible" $
    doubleInverses fromA (toA :: Point f c e q r -> Point f 'Affine e q r)
  , testProperty "affine transformation is doubly homeomorphic" $
    doubleHomeomorphism (flip mul 3) (flip mul 3) fromA (toA :: Point f c e q r -> Point f 'Affine e q r)
  ]

test :: forall f c e q r . (Curve f c e q r, Curve f 'Affine e q r)
  => TestName -> Point f c e q r -> Natural -> Natural -> Natural -> TestTree
test s g h q r = testGroup s
  [ testField "Field of points" (witness :: q)
  , testField "Field of coefficients" (witness :: r)
  , testGroup "Group axioms" $
    groupAxioms (<>) invert (mempty :: Point f c e q r) (const True)
  , curveAxioms g h q r
  ]
