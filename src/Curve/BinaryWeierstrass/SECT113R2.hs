module Curve.BinaryWeierstrass.SECT113R2
  -- | Types
  ( F2m
  , P
  -- | Parameters
  , _a
  , _b
  , _f
  , _g
  , _h
  , _n
  ) where

import Protolude

import BinaryField (BinaryField)

import Curve.BinaryWeierstrass (BWCurve(..), BWPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT113R2 curve
data SECT113R2

-- | Field of SECT113R2 curve
type F2m = BinaryField 0x20000000000000000000000000201

-- | SECT113R2 curve is a binary Weierstrass curve
instance BWCurve SECT113R2 F2m where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT113R2 curve
type P = BWPoint SECT113R2 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT113R2 curve
_a :: F2m
_a = 0x689918dbec7e5a0dd6dfc0aa55c7
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT113R2 curve
_b :: F2m
_b = 0x95e9a9ec9b297bd4bf36e059184f
{-# INLINE _b #-}

-- | Polynomial of SECT113R2 curve
_f :: Integer
_f = 0x20000000000000000000000000201
{-# INLINE _f #-}

-- | Generator of SECT113R2 curve
_g :: P
_g = A
     0x1a57a6a7b26ca5ef52fcdb8164797
     0xb3adc94ed1fe674c06e695baba1d
{-# INLINE _g #-}

-- | Cofactor of SECT113R2 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT113R2 curve
_n :: Integer
_n = 0x10000000000000108789b2496af93
{-# INLINE _n #-}
