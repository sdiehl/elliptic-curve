module Bench.Curve where

import Criterion.Main
import Data.Curve
import GHC.Base

benchmark :: Curve f c e q r => String -> Point f c e q r -> Benchmark
benchmark = (. nf (flip mul' (-3 :: Int))) . bench
