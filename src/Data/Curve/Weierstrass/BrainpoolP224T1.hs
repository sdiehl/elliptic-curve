module Data.Curve.Weierstrass.BrainpoolP224T1
  ( module Data.Curve.Weierstrass
  , module Data.Curve.Weierstrass.BrainpoolP224T1
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Data.Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP224T1 curve.
data BrainpoolP224T1

-- | Field of points of BrainpoolP224T1 curve.
type Fq = Prime 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff

-- | Field of coefficients of BrainpoolP224T1 curve.
type Fr = Prime 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f

-- | BrainpoolP224T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP224T1 Fq Fr => WCurve c BrainpoolP224T1 Fq Fr where
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

-- | Affine BrainpoolP224T1 curve point.
type PA = WAPoint BrainpoolP224T1 Fq Fr

-- | Affine BrainpoolP224T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP224T1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BrainpoolP224T1 point.
type PJ = WJPoint BrainpoolP224T1 Fq Fr

-- | Jacobian BrainpoolP224T1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP224T1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BrainpoolP224T1 point.
type PP = WPPoint BrainpoolP224T1 Fq Fr

-- | Projective BrainpoolP224T1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP224T1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP224T1 curve.
_a :: Fq
_a = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0fc
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BrainpoolP224T1 curve.
_b :: Fq
_b = 0x4b337d934104cd7bef271bf60ced1ed20da14c08b3bb64f18a60888d
{-# INLINABLE _b #-}

-- | Cofactor of BrainpoolP224T1 curve.
_h :: Natural
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BrainpoolP224T1 curve.
_q :: Natural
_q = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff
{-# INLINABLE _q #-}

-- | Order of BrainpoolP224T1 curve.
_r :: Natural
_r = 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BrainpoolP224T1 curve.
_x :: Fq
_x = 0x6ab1e344ce25ff3896424e7ffe14762ecb49f8928ac0c76029b4d580
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BrainpoolP224T1 curve.
_y :: Fq
_y = 0x374e9f5143e568cd23f3f4d7c0d4b1e41c8cc0d1c6abd5f1a46db4c
{-# INLINABLE _y #-}

-- | Generator of affine BrainpoolP224T1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BrainpoolP224T1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BrainpoolP224T1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
