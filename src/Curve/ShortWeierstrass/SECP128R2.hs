module Curve.ShortWeierstrass.SECP128R2
  -- | Imports
  ( Point(..)
  , SWCurve(..)
  , SWPoint
  -- | Types
  , Fq
  , P
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP128R2 curve
data SECP128R2

-- | Field of SECP128R2 curve
type Fq = PrimeField 0xFFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFF

-- | SECP128R2 curve is a short Weierstrass curve
instance SWCurve SECP128R2 Fq where
  _a _   = 0xD6031998D1B3BBFEBF59CC9BBFF9AEE1
  {-# INLINE _a #-}
  _b _   = 0x5EEEFCA380D02919DC2C6558BB6D8A5D
  {-# INLINE _b #-}
  _g     = notImplemented
  {-# INLINE _g #-}
  _h _ _ = 4
  {-# INLINE _h #-}
  _q _ _ = 0xFFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFF
  {-# INLINE _q #-}
  _r _ _ = 0x3FFFFFFF7FFFFFFFBE0024720613B5A3
  {-# INLINE _r #-}

-- | Point of SECP128R2 curve
type P = SWPoint SECP128R2 Fq
