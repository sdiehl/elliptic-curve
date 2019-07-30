module Curve.Montgomery.Curve448
  ( AP
  , Curve(..)
  , Fq
  , Fr
  , Group(..)
  , MCurve(..)
  , MPoint
  , MACurve(..)
  , MAPoint
  , Point(..)
  , _a
  , _b
  , _h
  , _q
  , _r
  , gA
  , xA
  , yA
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Montgomery (MCurve(..), MPoint, MACurve(..), MAPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Curve448 curve
-------------------------------------------------------------------------------

-- | Curve448 curve.
data Curve448

-- | Field of points of Curve448 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff

-- | Field of coefficients of Curve448 curve.
type Fr = PrimeField 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3

-- | Curve448 curve is a Montgomery curve.
instance Curve 'Montgomery c Curve448 Fq => MCurve c Curve448 Fq where
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

-- | Coefficient @A@ of Curve448 curve.
_a :: Fq
_a = 0x262a6
{-# INLINE _a #-}

-- | Coefficient @B@ of Curve448 curve.
_b :: Fq
_b = 0x1
{-# INLINE _b #-}

-- | Cofactor of Curve448 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Characteristic of Curve448 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff
{-# INLINE _q #-}

-- | Order of Curve448 curve.
_r :: Integer
_r = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine Curve448 point.
type AP = MAPoint Curve448 Fq

-- | Affine Curve448 curve is a Montgomery affine curve.
instance MACurve Curve448 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}
  xA_ = const xA
  {-# INLINE xA_ #-}
  yA_ = const yA
  {-# INLINE yA_ #-}

-- | Generator of affine Curve448 curve.
gA :: AP
gA = A xA yA
{-# INLINE gA #-}

-- | Coordinate @X@ of affine Curve448 curve.
xA :: Fq
xA = 0x5
{-# INLINE xA #-}

-- | Coordinate @Y@ of affine Curve448 curve.
yA :: Fq
yA = 0x7d235d1295f5b1f66c98ab6e58326fcecbae5d34f55545d060f75dc28df3f6edb8027e2346430d211312c4b150677af76fd7223d457b5b1a
{-# INLINE yA #-}
