module Main where

import Protolude

import Test.Tasty

import Test.Binary
import Test.Edwards
import Test.Field
import Test.Montgomery
import Test.Weierstrass

main :: IO ()
main = defaultMain $
  testGroup "Tests" [testBinary, testEdwards, testMontgomery, testWeierstrass, testField]
