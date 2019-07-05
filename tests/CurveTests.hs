module CurveTests where

import Protolude

import Curve
import GaloisField
import Test.Tasty
import Test.Tasty.QuickCheck

identities :: Eq a => (a -> a -> a) -> a -> a -> Bool
identities op e x = op x e == x && op e x == x

inverses :: Eq a => (a -> a -> a) -> (a -> a) -> a -> a -> Bool
inverses op inv e x = op x (inv x) == e && op (inv x) x == e

commutativity :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutativity op x y = op x y == op y x

associativity :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associativity op x y z = op x (op y z) == op (op x y) z

groupAxioms :: forall r c k .
  (Arbitrary (Point r c k), Curve r c k, Eq (Point r c k), GaloisField k, Show (Point r c k))
  => Proxy r -> Proxy c -> Proxy k -> TestName -> TestTree
groupAxioms _ _ _ str = testGroup ("Test group axioms of " <> str)
  [ testProperty "identity"
    $ identities ((<>) :: Point r c k -> Point r c k -> Point r c k) mempty
  , testProperty "inverses"
    $ inverses ((<>) :: Point r c k -> Point r c k -> Point r c k) inv mempty
  , testProperty "commutativity"
    $ commutativity ((<>) :: Point r c k -> Point r c k -> Point r c k)
  , testProperty "associativity"
    $ associativity ((<>) :: Point r c k -> Point r c k -> Point r c k)
  ]
