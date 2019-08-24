module Main where

import Protolude

import Criterion.Main

import Bench.Binary
import Bench.Edwards
import Bench.Montgomery
import Bench.Weierstrass
import Bench.Field

main :: IO ()
main = defaultMain
  [benchBinary, benchEdwards, benchMontgomery, benchWeierstrass, benchField]
