module CurveBenchmarks where

import Criterion.Main
import Curve
import GHC.Base

benchmark :: forall g . Group g => String -> g -> Benchmark
benchmark = (. whnf (flip mul 3)) . bench
