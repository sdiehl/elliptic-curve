module Curve.Weierstrass.BN384
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

-- | BN384 curve.
data BN384

-- | Field of points of BN384 curve.
type Fq = PrimeField 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca024c8fbe29531892c79534f9d306328261550a7cabd7cccd10b

-- | Field of coefficients of BN384 curve.
type Fr = PrimeField 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca023c8fbe29531892c795356487d8ac63e4f4db17384341a5775

-- | BN384 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN384 Fq Fr => WCurve c BN384 Fq Fr where
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

-- | Affine BN384 curve point.
type PA = WAPoint BN384 Fq Fr

-- | Affine BN384 curve is a Weierstrass affine curve.
instance WACurve BN384 Fq Fr where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BN384 point.
type PJ = WJPoint BN384 Fq Fr

-- | Jacobian BN384 curve is a Weierstrass Jacobian curve.
instance WJCurve BN384 Fq Fr where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BN384 point.
type PP = WPPoint BN384 Fq Fr

-- | Projective BN384 curve is a Weierstrass projective curve.
instance WPCurve BN384 Fq Fr where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN384 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN384 curve.
_b :: Fq
_b = 0x3
{-# INLINE _b #-}

-- | Cofactor of BN384 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN384 curve.
_q :: Integer
_q = 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca024c8fbe29531892c79534f9d306328261550a7cabd7cccd10b
{-# INLINE _q #-}

-- | Order of BN384 curve.
_r :: Integer
_r = 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca023c8fbe29531892c795356487d8ac63e4f4db17384341a5775
{-# INLINE _r #-}

-- | Coordinate @X@ of BN384 curve.
_x :: Fq
_x = 0x1
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN384 curve.
_y :: Fq
_y = 0x2
{-# INLINE _y #-}

-- | Generator of affine BN384 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of Jacobian BN384 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINE gJ #-}

-- | Generator of projective BN384 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
