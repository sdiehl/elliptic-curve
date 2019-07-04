module Curve.ShortWeierstrass.SECP128R1
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

-- | SECP128R1 curve
data SECP128R1

-- | Field of SECP128R1 curve
type Fq = PrimeField 0xFFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFF

-- | SECP128R1 curve is a short Weierstrass curve
instance SWCurve SECP128R1 Fq where
  _a _   = 0xFFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFC
  {-# INLINE _a #-}
  _b _   = 0xE87579C11079F43DD824993C2CEE5ED3
  {-# INLINE _b #-}
  _g     = notImplemented
  {-# INLINE _g #-}
  _h _ _ = 1
  {-# INLINE _h #-}
  _q _ _ = 0xFFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFF
  {-# INLINE _q #-}
  _r _ _ = 0xFFFFFFFE0000000075A30D1B9038A115
  {-# INLINE _r #-}

-- | Point of SECP128R1 curve
type P = SWPoint SECP128R1 Fq
