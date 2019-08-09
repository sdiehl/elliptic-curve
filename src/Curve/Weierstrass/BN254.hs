module Curve.Weierstrass.BN254
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

-- | BN254 curve.
data BN254

-- | Field of points of BN254 curve.
type Fq = PrimeField 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47

-- | Field of coefficients of BN254 curve.
type Fr = PrimeField 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001

-- | BN254 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254 Fq Fr => WCurve c BN254 Fq Fr where
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

-- | Affine BN254 curve point.
type PA = WAPoint BN254 Fq Fr

-- | Affine BN254 curve is a Weierstrass affine curve.
instance WACurve BN254 Fq Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BN254 point.
type PJ = WJPoint BN254 Fq Fr

-- | Jacobian BN254 curve is a Weierstrass Jacobian curve.
instance WJCurve BN254 Fq Fr where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BN254 point.
type PP = WPPoint BN254 Fq Fr

-- | Projective BN254 curve is a Weierstrass projective curve.
instance WPCurve BN254 Fq Fr where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254 curve.
_b :: Fq
_b = 0x3
{-# INLINE _b #-}

-- | Cofactor of BN254 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN254 curve.
_q :: Integer
_q = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINE _q #-}

-- | Order of BN254 curve.
_r :: Integer
_r = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
{-# INLINE _r #-}

-- | Coordinate @X@ of BN254 curve.
_x :: Fq
_x = 0x1
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN254 curve.
_y :: Fq
_y = 0x2
{-# INLINE _y #-}

-- | Generator of affine BN254 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of Jacobian BN254 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINE gJ #-}

-- | Generator of projective BN254 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
