module FieldTests where

import Protolude

import Curve.Field
import qualified Curve.Field.BN254TF as BN254TF
import Test.Tasty

import CurveTests

testField :: TestTree
testField = testGroup "Field"
  [ test' "BN254TF" (witness :: FGroup BN254TF.Fp12) BN254TF._p
  ]
