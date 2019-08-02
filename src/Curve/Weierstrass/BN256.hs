module Curve.Weierstrass.BN256
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , PA
  , PJ
  , PP
  , Point(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , WJCurve(..)
  , WJPoint
  , WPCurve(..)
  , WPPoint
  , _a
  , _b
  , _h
  , _q
  , _r
  , _x
  , _y
  , fromAtoJ
  , fromAtoP
  , fromJtoA
  , fromPtoA
  , gA
  , gJ
  , gP
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN256 curve.
data BN256

-- | Field of points of BN256 curve.
type Fq = PrimeField 0xfffffffffffcf0cd46e5f25eee71a49f0cdc65fb12980a82d3292ddbaed33013

-- | Field of coefficients of BN256 curve.
type Fr = PrimeField 0xfffffffffffcf0cd46e5f25eee71a49e0cdc65fb1299921af62d536cd10b500d

-- | BN256 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN256 Fq => WCurve c BN256 Fq where
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

-- | Affine BN256 curve point.
type PA = WAPoint BN256 Fq

-- | Affine BN256 curve is a Weierstrass affine curve.
instance WACurve BN256 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BN256 point.
type PJ = WJPoint BN256 Fq

-- | Jacobian BN256 curve is a Weierstrass Jacobian curve.
instance WJCurve BN256 Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BN256 point.
type PP = WPPoint BN256 Fq

-- | Projective BN256 curve is a Weierstrass projective curve.
instance WPCurve BN256 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN256 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN256 curve.
_b :: Fq
_b = 0x3
{-# INLINE _b #-}

-- | Cofactor of BN256 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN256 curve.
_q :: Integer
_q = 0xfffffffffffcf0cd46e5f25eee71a49f0cdc65fb12980a82d3292ddbaed33013
{-# INLINE _q #-}

-- | Order of BN256 curve.
_r :: Integer
_r = 0xfffffffffffcf0cd46e5f25eee71a49e0cdc65fb1299921af62d536cd10b500d
{-# INLINE _r #-}

-- | Coordinate @X@ of BN256 curve.
_x :: Fq
_x = 0x1
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN256 curve.
_y :: Fq
_y = 0x2
{-# INLINE _y #-}

-- | Generator of affine BN256 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of Jacobian BN256 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINE gJ #-}

-- | Generator of projective BN256 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
