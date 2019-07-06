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

import ExtensionField (IrreducibleMonic(..), x)

import Curve.BinaryWeierstrass (BWCurve(..), BWPoint, F2, Fm, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT113R2 curve
data SECT113R2

-- | Field of SECT113R2 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (113 :: Int) + x ^ (9 :: Int) + 1
type F2m = Fm FX

-- | SECT113R2 curve is a binary Weierstrass curve
instance BWCurve SECT113R2 FX where
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
_a = 0x00689918dbec7e5a0dd6dfc0aa55c7
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT113R2 curve
_b :: F2m
_b = 0x0095e9a9ec9b297bd4bf36e059184f
{-# INLINE _b #-}

-- | Polynomial of SECT113R2 curve
_f = split (witness :: F2m)
{-# INLINE _f #-}

-- | Generator of SECT113R2 curve
_g :: P
_g = A 0x01a57a6a7b26ca5ef52fcdb8164797
       0x00b3adc94ed1fe674c06e695baba1d
{-# INLINE _g #-}

-- | Cofactor of SECT113R2 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT113R2 curve
_n :: Integer
_n = 0x010000000000000108789b2496af93
{-# INLINE _n #-}
