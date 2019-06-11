-- To get the benchmarking data, run "stack bench".

module Main where

import Protolude

import Criterion.Main (bgroup, defaultMain)

import Bench (benchmarks)

main :: IO ()
main = defaultMain [bgroup "JubJub" benchmarks]
