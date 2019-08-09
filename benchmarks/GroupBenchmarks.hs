module GroupBenchmarks where

import Criterion.Main
import GHC.Base
import Group

benchmark :: Group g => String -> g -> Benchmark
benchmark = (. whnf (flip mul' 3)) . bench
