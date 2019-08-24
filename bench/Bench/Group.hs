module Bench.Group where

import Criterion.Main
import GHC.Base
import Data.Group

benchmark :: Group g => String -> g -> Benchmark
benchmark = (. nf (flip mul' 3)) . bench
