module Curve.ShortWeierstrass.SECP224R1
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

-- | SECP224R1 curve
data SECP224R1

-- | Field of SECP224R1 curve
type Fq = PrimeField 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001

-- | SECP224R1 curve is a short Weierstrass curve
instance SWCurve SECP224R1 Fq where
  _a _ = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE
  {-# INLINE _a #-}
  _b _ = 0xB4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4
  {-# INLINE _b #-}
  _g   = notImplemented
  {-# INLINE _g #-}
  _h _ = 1 
  {-# INLINE _h #-}
  _q _ = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001
  {-# INLINE _q #-}
  _r _ = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D
  {-# INLINE _r #-}

-- | Point of SECP224R1 curve
type P = SWPoint SECP224R1 Fq
