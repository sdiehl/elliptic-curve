module Curve.Weierstrass.ANSSIFRP256V1
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

-- | ANSSIFRP256V1 curve.
data ANSSIFRP256V1

-- | Field of points of ANSSIFRP256V1 curve.
type Fq = PrimeField 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c03

-- | Field of coefficients of ANSSIFRP256V1 curve.
type Fr = PrimeField 0xf1fd178c0b3ad58f10126de8ce42435b53dc67e140d2bf941ffdd459c6d655e1

-- | ANSSIFRP256V1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c ANSSIFRP256V1 Fq => WCurve c ANSSIFRP256V1 Fq where
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

-- | Affine ANSSIFRP256V1 curve point.
type PA = WAPoint ANSSIFRP256V1 Fq

-- | Affine ANSSIFRP256V1 curve is a Weierstrass affine curve.
instance WACurve ANSSIFRP256V1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective ANSSIFRP256V1 point.
type PP = WPPoint ANSSIFRP256V1 Fq

-- | Projective ANSSIFRP256V1 curve is a Weierstrass projective curve.
instance WPCurve ANSSIFRP256V1 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of ANSSIFRP256V1 curve.
_a :: Fq
_a = 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c00
{-# INLINE _a #-}

-- | Coefficient @B@ of ANSSIFRP256V1 curve.
_b :: Fq
_b = 0xee353fca5428a9300d4aba754a44c00fdfec0c9ae4b1a1803075ed967b7bb73f
{-# INLINE _b #-}

-- | Cofactor of ANSSIFRP256V1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of ANSSIFRP256V1 curve.
_q :: Integer
_q = 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c03
{-# INLINE _q #-}

-- | Order of ANSSIFRP256V1 curve.
_r :: Integer
_r = 0xf1fd178c0b3ad58f10126de8ce42435b53dc67e140d2bf941ffdd459c6d655e1
{-# INLINE _r #-}

-- | Coordinate @X@ of ANSSIFRP256V1 curve.
_x :: Fq
_x = 0xb6b3d4c356c139eb31183d4749d423958c27d2dcaf98b70164c97a2dd98f5cff
{-# INLINE _x #-}

-- | Coordinate @Y@ of ANSSIFRP256V1 curve.
_y :: Fq
_y = 0x6142e0f7c8b204911f9271f0f3ecef8c2701c307e8e4c9e183115a1554062cfb
{-# INLINE _y #-}

-- | Affine generator of ANSSIFRP256V1 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Projective generator of ANSSIFRP256V1 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
