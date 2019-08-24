module Test.Field where

import qualified Group.Field.BN254TF as BN254TF
import Test.Tasty

import Test.Group

testField :: TestTree
testField = testGroup "Field"
  [ test' "BN254TF" BN254TF._g BN254TF._h BN254TF._q BN254TF._r
  ]
