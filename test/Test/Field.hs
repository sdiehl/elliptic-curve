module Test.Field where

import Protolude

import Data.Field.Galois
import Test.Tasty
import Test.Tasty.QuickCheck

annihilation :: Eq a => (a -> a -> a) -> a -> a -> Bool
annihilation op e x = op x e == e && op e x == e

associativity :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associativity op x y z = op x (op y z) == op (op x y) z

commutativity :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutativity op x y = op x y == op y x

distributivity :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
distributivity op op' x y z = op (op' x y) z == op' (op x z) (op y z)
                           && op x (op' y z) == op' (op x y) (op x z)

identities :: Eq a => (a -> a -> a) -> a -> a -> Bool
identities op e x = op x e == x && op e x == x

inverses :: Eq a => (a -> a -> a) -> (a -> a) -> a -> a -> Bool
inverses op inv e x = op x (inv x) == e && op (inv x) x == e

groupAxioms :: forall g . (Arbitrary g, Eq g, Show g)
  => (g -> g -> g) -> (g -> g) -> g -> (g -> Bool) -> [TestTree]
groupAxioms add inv id cond =
  [ testProperty "associativity" $
    associativity add
  , testProperty "commutativity" $
    commutativity add
  , testProperty "identity" $
    identities add id
  , testProperty "inverses" $
    \x -> cond x ==> inverses add inv id x
  ]

fieldAxioms :: forall k . GaloisField k => k -> TestTree
fieldAxioms _ = testGroup "Field axioms"
  [ testGroup "additive group axioms" $
    groupAxioms (+) negate (0 :: k) (const True)
  , testGroup "multiplicative group axioms" $
    groupAxioms (*) recip (1 :: k) (/= 0)
  , testProperty "distributivity of multiplication over addition" $
    distributivity ((*) :: k -> k -> k) (+)
  , testProperty "multiplicative annihilation" $
    annihilation ((*) :: k -> k -> k) 0
  ]
