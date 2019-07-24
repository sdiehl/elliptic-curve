module FieldTests where

import qualified Curve.Field.BN254TF as BN254TF
import Test.Tasty

import CurveTests

testField :: TestTree
testField = testGroup "Field"
  [ test' "BN254TF" BN254TF._g BN254TF._n BN254TF._p
  ]
