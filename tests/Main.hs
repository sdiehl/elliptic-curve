module Main where

import Protolude

import Test.Tasty

import BinaryTests
import EdwardsTests
import FieldTests
import MontgomeryTests
import WeierstrassTests

main :: IO ()
main = defaultMain $
  testGroup "Tests" [testBinary, testEdwards, testMontgomery, testWeierstrass, testField]
