module Curve.Weierstrass.SECP128R1
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

-- | SECP128R1 curve.
data SECP128R1

-- | Field of points of SECP128R1 curve.
type Fq = PrimeField 0xfffffffdffffffffffffffffffffffff

-- | Field of coefficients of SECP128R1 curve.
type Fr = PrimeField 0xfffffffe0000000075a30d1b9038a115

-- | SECP128R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP128R1 Fq => WCurve c SECP128R1 Fq where
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

-- | Affine SECP128R1 curve point.
type PA = WAPoint SECP128R1 Fq

-- | Affine SECP128R1 curve is a Weierstrass affine curve.
instance WACurve SECP128R1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian SECP128R1 point.
type PJ = WJPoint SECP128R1 Fq

-- | Jacobian SECP128R1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP128R1 Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective SECP128R1 point.
type PP = WPPoint SECP128R1 Fq

-- | Projective SECP128R1 curve is a Weierstrass projective curve.
instance WPCurve SECP128R1 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP128R1 curve.
_a :: Fq
_a = 0xfffffffdfffffffffffffffffffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP128R1 curve.
_b :: Fq
_b = 0xe87579c11079f43dd824993c2cee5ed3
{-# INLINE _b #-}

-- | Cofactor of SECP128R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP128R1 curve.
_q :: Integer
_q = 0xfffffffdffffffffffffffffffffffff
{-# INLINE _q #-}

-- | Order of SECP128R1 curve.
_r :: Integer
_r = 0xfffffffe0000000075a30d1b9038a115
{-# INLINE _r #-}

-- | Coordinate @X@ of SECP128R1 curve.
_x :: Fq
_x = 0x161ff7528b899b2d0c28607ca52c5b86
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP128R1 curve.
_y :: Fq
_y = 0xcf5ac8395bafeb13c02da292dded7a83
{-# INLINE _y #-}

-- | Generator of affine SECP128R1 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Generator of Jacobian SECP128R1 curve.
gJ :: PJ
gJ = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gJ #-}

-- | Generator of projective SECP128R1 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
