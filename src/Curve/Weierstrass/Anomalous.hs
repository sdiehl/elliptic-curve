module Curve.Weierstrass.Anomalous
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , PA
  , PP
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , WPCurve(..)
  , WPPoint
  , _a
  , _b
  , _h
  , _q
  , _r
  , _x
  , _y
  , gA
  , gP
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Anomalous curve.
data Anomalous

-- | Field of points of Anomalous curve.
type Fq = PrimeField 0xb0000000000000000000000953000000000000000000001f9d7

-- | Field of coefficients of Anomalous curve.
type Fr = PrimeField 0xb0000000000000000000000953000000000000000000001f9d7

-- | Anomalous curve is a Weierstrass curve.
instance Curve 'Weierstrass c Anomalous Fq => WCurve c Anomalous Fq where
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine Anomalous curve point.
type PA = WAPoint Anomalous Fq

-- | Affine Anomalous curve is a Weierstrass affine curve.
instance WACurve Anomalous Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective Anomalous point.
type PP = WPPoint Anomalous Fq

-- | Projective Anomalous curve is a Weierstrass projective curve.
instance WPCurve Anomalous Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Anomalous curve.
_a :: Fq
_a = 0x98d0fac687d6343eb1a1f595283eb1a1f58d0fac687d635f5e4
{-# INLINE _a #-}

-- | Coefficient @B@ of Anomalous curve.
_b :: Fq
_b = 0x4a1f58d0fac687d6343eb1a5e2d6343eb1a1f58d0fac688ab3f
{-# INLINE _b #-}

-- | Cofactor of Anomalous curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of Anomalous curve.
_q :: Integer
_q = 0xb0000000000000000000000953000000000000000000001f9d7
{-# INLINE _q #-}

-- | Order of Anomalous curve.
_r :: Integer
_r = 0xb0000000000000000000000953000000000000000000001f9d7
{-# INLINE _r #-}

-- | Coordinate @X@ of Anomalous curve.
_x :: Fq
_x = 0x101efb35fd1963c4871a2d17edaafa7e249807f58f8705126c6
{-# INLINE _x #-}

-- | Coordinate @Y@ of Anomalous curve.
_y :: Fq
_y = 0x22389a3954375834304ba1d509a97de6c07148ea7f5951b20e7
{-# INLINE _y #-}

-- | Affine generator of Anomalous curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Projective generator of Anomalous curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
