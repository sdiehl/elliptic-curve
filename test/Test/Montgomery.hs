module Test.Montgomery where

import Protolude

import qualified Data.Curve.Montgomery.Curve448    as Curve448
import qualified Data.Curve.Montgomery.Curve25519  as Curve25519
import qualified Data.Curve.Montgomery.Curve383187 as Curve383187
import qualified Data.Curve.Montgomery.M221        as M221
import qualified Data.Curve.Montgomery.M383        as M383
import qualified Data.Curve.Montgomery.M511        as M511
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Curve

testMontgomery :: TestTree
testMontgomery = testGroup "Montgomery"
  [ testMontgomery' 100    "Curve448"    Curve448._h    Curve448._q    Curve448._r    Curve448.gA
  , testMontgomery' 100  "Curve25519"  Curve25519._h  Curve25519._q  Curve25519._r  Curve25519.gA
  , testMontgomery' 100 "Curve383187" Curve383187._h Curve383187._q Curve383187._r Curve383187.gA
  , testMontgomery' 100        "M221"        M221._h        M221._q        M221._r        M221.gA
  , testMontgomery' 100        "M383"        M383._h        M383._q        M383._r        M383.gA
  , testMontgomery' 100        "M511"        M511._h        M511._q        M511._r        M511.gA
  ]
  where
    testMontgomery' n c h q r a = localOption (QuickCheckTests n) $ testGroup c
      [ test "Affine" a h q r
      ]
