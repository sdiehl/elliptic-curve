module Main where

import Protolude

import Criterion.Main

import BinaryBenchmarks
import EdwardsBenchmarks
import MontgomeryBenchmarks
import WeierstrassBenchmarks

main :: IO ()
main = defaultMain
  [benchmarkBinary, benchmarkEdwards, benchmarkMontgomery, benchmarkWeierstrass]
