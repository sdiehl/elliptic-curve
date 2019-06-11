module Params (_q, _r, _h, _d) where

import Protolude

import Pairing.Fq (Fq)

-- | Characteristic of field
_q :: Integer
_q = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

-- | Order of curve
_r :: Integer
_r = 0x0e7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7

-- | Cofactor of curve
_h :: Integer
_h = 8

-- | Coefficient of curve
_d :: Fq
_d = -(10240 / 10241)
