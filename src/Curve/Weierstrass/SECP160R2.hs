module Curve.Weierstrass.SECP160R2
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
  , _a
  , _b
  , _g
  , _h
  , _q
  , _r
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP160R2 curve.
data SECP160R2

-- | Field of points of SECP160R2 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffeffffac73

-- | Field of coefficients of SECP160R2 curve.
type Fr = PrimeField 0x100000000000000000000351ee786a818f3a1a16b

-- | SECP160R2 curve is a Weierstrass curve.
instance WCurve SECP160R2 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Point of SECP160R2 curve.
type P = WPoint SECP160R2 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP160R2 curve.
_a :: Fq
_a = 0xfffffffffffffffffffffffffffffffeffffac70
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP160R2 curve.
_b :: Fq
_b = 0xb4e134d3fb59eb8bab57274904664d5af50388ba
{-# INLINE _b #-}

-- | Generator of SECP160R2 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECP160R2 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP160R2 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffeffffac73
{-# INLINE _q #-}

-- | Order of SECP160R2 curve.
_r :: Integer
_r = 0x100000000000000000000351ee786a818f3a1a16b
{-# INLINE _r #-}

-- | Coordinate @X@ of SECP160R2 curve.
_x :: Fq
_x = 0x52dcb034293a117e1f4ff11b30f7199d3144ce6d
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP160R2 curve.
_y :: Fq
_y = 0xfeaffef2e331f296e071fa0df9982cfea7d43f2e
{-# INLINE _y #-}
