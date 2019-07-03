module Curve.ShortWeierstrass.BLS12_381
  -- | Parameters
  ( _q
  , _r
  ) where

import Protolude

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Characteristic of BLS12-381 curve
_q :: Integer
_q = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
{-# INLINE _q #-}

-- | Order of BLS12-381 curve
_r :: Integer
_r = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
{-# INLINE _r #-}
