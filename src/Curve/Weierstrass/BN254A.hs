module Curve.Weierstrass.BN254A
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

-- | BN254A curve.
data BN254A

-- | Field of points of BN254A curve.
type Fq = PrimeField 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001

-- | Field of coefficients of BN254A curve.
type Fr = PrimeField 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001

-- | BN254A curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254A Fq => WCurve c BN254A Fq where
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

-- | Affine BN254A curve point.
type PA = WAPoint BN254A Fq

-- | Affine BN254A curve is a Weierstrass affine curve.
instance WACurve BN254A Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BN254A point.
type PJ = WJPoint BN254A Fq

-- | Jacobian BN254A curve is a Weierstrass Jacobian curve.
instance WJCurve BN254A Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BN254A point.
type PP = WPPoint BN254A Fq

-- | Projective BN254A curve is a Weierstrass projective curve.
instance WPCurve BN254A Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254A curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254A curve.
_b :: Fq
_b = 0x5
{-# INLINE _b #-}

-- | Cofactor of BN254A curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN254A curve.
_q :: Integer
_q = 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001
{-# INLINE _q #-}

-- | Order of BN254A curve.
_r :: Integer
_r = 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001
{-# INLINE _r #-}

-- | Coordinate @X@ of BN254A curve.
_x :: Fq
_x = 0x1
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN254A curve.
_y :: Fq
_y = 0xd45589b158faaf6ab0e4ad38d998e9982e7ff63964ee1460342a592677cccb0
{-# INLINE _y #-}

-- | Generator of affine BN254A curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of Jacobian BN254A curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINE gJ #-}

-- | Generator of projective BN254A curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
