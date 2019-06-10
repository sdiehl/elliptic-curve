module JubJub where

import Protolude

-- http://hyperelliptic.org/EFD/g1p/auto-twisted.html

_q :: Integer
_q = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

_r :: Integer
_r = 0x0e7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7

_s :: Integer
_s = 0x0e7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
-- 6554484396890773809930967563523245729705921265872317281365359162392183254199

_h :: Integer
_h = 8

_d :: Rational
_d = -(10240/10241)
