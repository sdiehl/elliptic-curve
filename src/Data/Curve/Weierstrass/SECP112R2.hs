module Data.Curve.Weierstrass.SECP112R2
  ( module Data.Curve.Weierstrass
  , module Data.Curve.Weierstrass.SECP112R2
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP112R2 curve.
data SECP112R2

-- | Field of points of SECP112R2 curve.
type Fq = Prime 0xdb7c2abf62e35e668076bead208b

-- | Field of coefficients of SECP112R2 curve.
type Fr = Prime 0x36df0aafd8b8d7597ca10520d04b

-- | SECP112R2 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP112R2 Fq Fr => WCurve c SECP112R2 Fq Fr where
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

-- | Affine SECP112R2 curve point.
type PA = WAPoint SECP112R2 Fq Fr

-- | Affine SECP112R2 curve is a Weierstrass affine curve.
instance WACurve SECP112R2 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian SECP112R2 point.
type PJ = WJPoint SECP112R2 Fq Fr

-- | Jacobian SECP112R2 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP112R2 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective SECP112R2 point.
type PP = WPPoint SECP112R2 Fq Fr

-- | Projective SECP112R2 curve is a Weierstrass projective curve.
instance WPCurve SECP112R2 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP112R2 curve.
_a :: Fq
_a = 0x6127c24c05f38a0aaaf65c0ef02c
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECP112R2 curve.
_b :: Fq
_b = 0x51def1815db5ed74fcc34c85d709
{-# INLINABLE _b #-}

-- | Cofactor of SECP112R2 curve.
_h :: Integer
_h = 0x4
{-# INLINABLE _h #-}

-- | Characteristic of SECP112R2 curve.
_q :: Integer
_q = 0xdb7c2abf62e35e668076bead208b
{-# INLINABLE _q #-}

-- | Order of SECP112R2 curve.
_r :: Integer
_r = 0x36df0aafd8b8d7597ca10520d04b
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECP112R2 curve.
_x :: Fq
_x = 0x4ba30ab5e892b4e1649dd0928643
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECP112R2 curve.
_y :: Fq
_y = 0xadcd46f5882e3747def36e956e97
{-# INLINABLE _y #-}

-- | Generator of affine SECP112R2 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian SECP112R2 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective SECP112R2 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
