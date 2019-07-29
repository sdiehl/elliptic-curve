module Curve.Weierstrass.SECP192R1
  ( AP
  , Curve(..)
  , Fq
  , Fr
  , Group(..)
  , Point(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , _a
  , _b
  , _g
  , _h
  , _q
  , _r
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint, WACurve(..), WAPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- SECP192R1 curve
-------------------------------------------------------------------------------

-- | SECP192R1 curve.
data SECP192R1

-- | Field of points of SECP192R1 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffeffffffffffffffff

-- | Field of coefficients of SECP192R1 curve.
type Fr = PrimeField 0xffffffffffffffffffffffff99def836146bc9b1b4d22831

-- | SECP192R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP192R1 Fq => WCurve c SECP192R1 Fq where
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

-- | Coefficient @A@ of SECP192R1 curve.
_a :: Fq
_a = 0xfffffffffffffffffffffffffffffffefffffffffffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP192R1 curve.
_b :: Fq
_b = 0x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
{-# INLINE _b #-}

-- | Cofactor of SECP192R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP192R1 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffeffffffffffffffff
{-# INLINE _q #-}

-- | Order of SECP192R1 curve.
_r :: Integer
_r = 0xffffffffffffffffffffffff99def836146bc9b1b4d22831
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECP192R1 point.
type AP = WAPoint SECP192R1 Fq

-- | Affine SECP192R1 curve is a Weierstrass affine curve.
instance WACurve SECP192R1 Fq where
  g_ = _g
  {-# INLINE g_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Generator of affine SECP192R1 curve.
_g :: AP
_g = A _x _y
{-# INLINE _g #-}

-- | Coordinate @X@ of affine SECP192R1 curve.
_x :: Fq
_x = 0x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012
{-# INLINE _x #-}

-- | Coordinate @Y@ of affine SECP192R1 curve.
_y :: Fq
_y = 0x7192b95ffc8da78631011ed6b24cdd573f977a11e794811
{-# INLINE _y #-}
