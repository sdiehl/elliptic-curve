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

groupAxioms :: forall c f .
  (Arbitrary (P c f), Curve c f, Eq (P c f), GaloisField f, Show (P c f))
  => Proxy c -> Proxy f -> TestName -> TestTree
groupAxioms _ _ str = testGroup ("Test group axioms of " <> str)
  [ testProperty "identity"
    $ identities ((<>) :: P c f -> P c f -> P c f) mempty
  , testProperty "inverses"
    $ inverses ((<>) :: P c f -> P c f -> P c f) inv mempty
  , testProperty "commutativity"
    $ commutativity ((<>) :: P c f -> P c f -> P c f)
  , testProperty "associativity"
    $ associativity ((<>) :: P c f -> P c f -> P c f)
  ]
