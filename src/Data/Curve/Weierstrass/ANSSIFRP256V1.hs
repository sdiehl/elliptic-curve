module Data.Curve.Weierstrass.ANSSIFRP256V1
  ( module Data.Curve.Weierstrass
  , module Data.Curve.Weierstrass.ANSSIFRP256V1
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | ANSSIFRP256V1 curve.
data ANSSIFRP256V1

-- | Field of points of ANSSIFRP256V1 curve.
type Fq = Prime 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c03

-- | Field of coefficients of ANSSIFRP256V1 curve.
type Fr = Prime 0xf1fd178c0b3ad58f10126de8ce42435b53dc67e140d2bf941ffdd459c6d655e1

-- | ANSSIFRP256V1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c ANSSIFRP256V1 Fq Fr => WCurve c ANSSIFRP256V1 Fq Fr where
  a_ = const _a
  {-# INLINABLE a_ #-}
  b_ = const _b
  {-# INLINABLE b_ #-}
  h_ = const _h
  {-# INLINABLE h_ #-}
  q_ = const _q
  {-# INLINABLE q_ #-}
  r_ = const _r
  {-# INLINABLE r_ #-}
  x_ = const _x
  {-# INLINABLE x_ #-}
  y_ = const _y
  {-# INLINABLE y_ #-}

-- | Affine ANSSIFRP256V1 curve point.
type PA = WAPoint ANSSIFRP256V1 Fq Fr

-- | Affine ANSSIFRP256V1 curve is a Weierstrass affine curve.
instance WACurve ANSSIFRP256V1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian ANSSIFRP256V1 point.
type PJ = WJPoint ANSSIFRP256V1 Fq Fr

-- | Jacobian ANSSIFRP256V1 curve is a Weierstrass Jacobian curve.
instance WJCurve ANSSIFRP256V1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective ANSSIFRP256V1 point.
type PP = WPPoint ANSSIFRP256V1 Fq Fr

-- | Projective ANSSIFRP256V1 curve is a Weierstrass projective curve.
instance WPCurve ANSSIFRP256V1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of ANSSIFRP256V1 curve.
_a :: Fq
_a = 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c00
{-# INLINABLE _a #-}

-- | Coefficient @B@ of ANSSIFRP256V1 curve.
_b :: Fq
_b = 0xee353fca5428a9300d4aba754a44c00fdfec0c9ae4b1a1803075ed967b7bb73f
{-# INLINABLE _b #-}

-- | Cofactor of ANSSIFRP256V1 curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of ANSSIFRP256V1 curve.
_q :: Integer
_q = 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c03
{-# INLINABLE _q #-}

-- | Order of ANSSIFRP256V1 curve.
_r :: Integer
_r = 0xf1fd178c0b3ad58f10126de8ce42435b53dc67e140d2bf941ffdd459c6d655e1
{-# INLINABLE _r #-}

-- | Coordinate @X@ of ANSSIFRP256V1 curve.
_x :: Fq
_x = 0xb6b3d4c356c139eb31183d4749d423958c27d2dcaf98b70164c97a2dd98f5cff
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of ANSSIFRP256V1 curve.
_y :: Fq
_y = 0x6142e0f7c8b204911f9271f0f3ecef8c2701c307e8e4c9e183115a1554062cfb
{-# INLINABLE _y #-}

-- | Generator of affine ANSSIFRP256V1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian ANSSIFRP256V1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective ANSSIFRP256V1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
