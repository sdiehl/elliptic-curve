module Curve.Weierstrass.SECP224K1
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

-- | SECP224K1 curve.
data SECP224K1

-- | Field of points of SECP224K1 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffeffffe56d

-- | Field of coefficients of SECP224K1 curve.
type Fr = PrimeField 0x10000000000000000000000000001dce8d2ec6184caf0a971769fb1f7

-- | SECP224K1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP224K1 Fq => WCurve c SECP224K1 Fq where
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

-- | Affine SECP224K1 curve point.
type PA = WAPoint SECP224K1 Fq

-- | Affine SECP224K1 curve is a Weierstrass affine curve.
instance WACurve SECP224K1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian SECP224K1 point.
type PJ = WJPoint SECP224K1 Fq

-- | Jacobian SECP224K1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP224K1 Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective SECP224K1 point.
type PP = WPPoint SECP224K1 Fq

-- | Projective SECP224K1 curve is a Weierstrass projective curve.
instance WPCurve SECP224K1 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP224K1 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP224K1 curve.
_b :: Fq
_b = 0x5
{-# INLINE _b #-}

-- | Cofactor of SECP224K1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP224K1 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffeffffe56d
{-# INLINE _q #-}

-- | Order of SECP224K1 curve.
_r :: Integer
_r = 0x10000000000000000000000000001dce8d2ec6184caf0a971769fb1f7
{-# INLINE _r #-}

-- | Coordinate @X@ of SECP224K1 curve.
_x :: Fq
_x = 0xa1455b334df099df30fc28a169a467e9e47075a90f7e650eb6b7a45c
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP224K1 curve.
_y :: Fq
_y = 0x7e089fed7fba344282cafbd6f7e319f7c0b0bd59e2ca4bdb556d61a5
{-# INLINE _y #-}

-- | Generator of affine SECP224K1 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Generator of Jacobian SECP224K1 curve.
gJ :: PJ
gJ = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gJ #-}

-- | Generator of projective SECP224K1 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
