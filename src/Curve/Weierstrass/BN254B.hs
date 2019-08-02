module Curve.Weierstrass.BN254B
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
  , gJ
  , gP
  , pattern A
  , pattern J
  , pattern P
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254B curve.
data BN254B

-- | Field of points of BN254B curve.
type Fq = PrimeField 0x2523648240000001ba344d80000000086121000000000013a700000000000013

-- | Field of coefficients of BN254B curve.
type Fr = PrimeField 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d

-- | BN254B curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254B Fq => WCurve c BN254B Fq where
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

-- | Affine BN254B curve point.
type PA = WAPoint BN254B Fq

-- | Affine BN254B curve is a Weierstrass affine curve.
instance WACurve BN254B Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BN254B point.
type PJ = WJPoint BN254B Fq

-- | Jacobian BN254B curve is a Weierstrass Jacobian curve.
instance WJCurve BN254B Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BN254B point.
type PP = WPPoint BN254B Fq

-- | Projective BN254B curve is a Weierstrass projective curve.
instance WPCurve BN254B Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254B curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254B curve.
_b :: Fq
_b = 0x2
{-# INLINE _b #-}

-- | Cofactor of BN254B curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN254B curve.
_q :: Integer
_q = 0x2523648240000001ba344d80000000086121000000000013a700000000000013
{-# INLINE _q #-}

-- | Order of BN254B curve.
_r :: Integer
_r = 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d
{-# INLINE _r #-}

-- | Coordinate @X@ of BN254B curve.
_x :: Fq
_x = 0x2523648240000001ba344d80000000086121000000000013a700000000000012
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN254B curve.
_y :: Fq
_y = 0x1
{-# INLINE _y #-}

-- | Generator of affine BN254B curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Generator of Jacobian BN254B curve.
gJ :: PJ
gJ = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gJ #-}

-- | Generator of projective BN254B curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
