{-# LANGUAGE DataKinds #-}

module Params
  ( Fq
  , _d
  , _h
  , _q
  , _r
  ) where

import Protolude

import GaloisField (GaloisField(..))
import PrimeField (PrimeField(..))

-- | Field type synonym
type Fq = PrimeField 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
-- type F = PrimeField _q

-- | Characteristic of field
_q :: Integer
_q = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
-- 52435875175126190479447740508185965837690552500527637822603658699938581184513

-- | Order of curve
_r :: Integer
_r = 0x0e7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
-- 6554484396890773809930967563523245729705921265872317281365359162392183254199

-- | Cofactor of curve
_h :: Integer
_h = 8

-- | Coefficient of curve
_d :: Fq
_d = -(10240 / 10241)
