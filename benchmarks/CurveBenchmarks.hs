module CurveBenchmarks where

import Criterion.Main
import Curve
import GHC.Base

benchmark :: forall r c k . Curve r c k => String -> Point r c k -> Benchmark
benchmark = (. whnf (flip mul (3 :: Int))) . bench
