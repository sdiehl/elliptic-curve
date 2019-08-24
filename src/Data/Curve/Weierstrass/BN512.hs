module Data.Curve.Weierstrass.BN512
  ( module Data.Curve.Weierstrass
  , module Data.Curve.Weierstrass.BN512
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN512 curve.
data BN512

-- | Field of points of BN512 curve.
type Fq = Prime 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef455146cf1eacbe98b8e48c65deab236fe1916a55ce5f4c6467b4eb280922adef33

-- | Field of coefficients of BN512 curve.
type Fr = Prime 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef445146cf1eacbe98b8e48c65deab2679a34a10313e04f9a2b406a64a5f519a09ed

-- | BN512 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN512 Fq Fr => WCurve c BN512 Fq Fr where
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

-- | Affine BN512 curve point.
type PA = WAPoint BN512 Fq Fr

-- | Affine BN512 curve is a Weierstrass affine curve.
instance WACurve BN512 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BN512 point.
type PJ = WJPoint BN512 Fq Fr

-- | Jacobian BN512 curve is a Weierstrass Jacobian curve.
instance WJCurve BN512 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BN512 point.
type PP = WPPoint BN512 Fq Fr

-- | Projective BN512 curve is a Weierstrass projective curve.
instance WPCurve BN512 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN512 curve.
_a :: Fq
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BN512 curve.
_b :: Fq
_b = 0x3
{-# INLINABLE _b #-}

-- | Cofactor of BN512 curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BN512 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef455146cf1eacbe98b8e48c65deab236fe1916a55ce5f4c6467b4eb280922adef33
{-# INLINABLE _q #-}

-- | Order of BN512 curve.
_r :: Integer
_r = 0xfffffffffffffffffffffffffff9ec7f01c60ba1d8cb5307c0bbe3c111b0ef445146cf1eacbe98b8e48c65deab2679a34a10313e04f9a2b406a64a5f519a09ed
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BN512 curve.
_x :: Fq
_x = 0x1
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BN512 curve.
_y :: Fq
_y = 0x2
{-# INLINABLE _y #-}

-- | Generator of affine BN512 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BN512 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BN512 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
