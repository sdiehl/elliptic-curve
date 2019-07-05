module Curve.ShortWeierstrass.SECP128R2
  -- | Imports
  ( Point(..)
  , SWCurve(..)
  , SWPoint
  -- | Types
  , Fp
  , P
  ) where

import PrimeField (PrimeField)

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP128R2 curve
data SECP128R2

-- | Field of SECP128R2 curve
type Fp = PrimeField 0xfffffffdffffffffffffffffffffffff

-- | SECP128R2 curve is a short Weierstrass curve
instance SWCurve SECP128R2 Fp where
  _a _ = 0xd6031998d1b3bbfebf59cc9bbff9aee1
  {-# INLINE _a #-}
  _b _ = 0x5eeefca380d02919dc2c6558bb6d8a5d
  {-# INLINE _b #-}
  _g   = A 0x7b6aa5d85e572983e6fb32a7cdebc140
           0x27b6916a894d3aee7106fe805fc34b44
  {-# INLINE _g #-}
  _h _ = 4
  {-# INLINE _h #-}
  _n _ = 0x3fffffff7fffffffbe0024720613b5a3
  {-# INLINE _n #-}
  _p _ = 0xfffffffdffffffffffffffffffffffff
  {-# INLINE _p #-}

-- | Point of SECP128R2 curve
type P = SWPoint SECP128R2 Fp
