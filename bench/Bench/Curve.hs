module Bench.Curve where

import Criterion.Main
import Data.Cyclic
import GHC.Base

benchmark :: Cyclic g => String -> g -> Benchmark
benchmark = (. nf (flip mul' 3)) . bench
