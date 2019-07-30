module Curve.Weierstrass.SECP160R2
  ( AP
  , Curve(..)
  , Fq
  , Fr
  , Group(..)
  , Point(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , _a
  , _b
  , _h
  , _q
  , _r
  , gA
  , xA
  , yA
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- SECP160R2 curve
-------------------------------------------------------------------------------

-- | SECP160R2 curve.
data SECP160R2

-- | Field of points of SECP160R2 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffeffffac73

-- | Field of coefficients of SECP160R2 curve.
type Fr = PrimeField 0x100000000000000000000351ee786a818f3a1a16b

-- | SECP160R2 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP160R2 Fq => WCurve c SECP160R2 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}

-- | Coefficient @A@ of SECP160R2 curve.
_a :: Fq
_a = 0xfffffffffffffffffffffffffffffffeffffac70
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP160R2 curve.
_b :: Fq
_b = 0xb4e134d3fb59eb8bab57274904664d5af50388ba
{-# INLINE _b #-}

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

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECP160R2 point.
type AP = WAPoint SECP160R2 Fq

-- | Affine SECP160R2 curve is a Weierstrass affine curve.
instance WACurve SECP160R2 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine SECP160R2 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine SECP160R2 curve.
xA :: Fq
xA = 0x52dcb034293a117e1f4ff11b30f7199d3144ce6d
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine SECP160R2 curve.
yA :: Fq
yA = 0xfeaffef2e331f296e071fa0df9982cfea7d43f2e
{-# INLINE yA #-}
