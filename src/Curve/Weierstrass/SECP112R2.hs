module Curve.Weierstrass.SECP112R2
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
  , gP
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP112R2 curve.
data SECP112R2

-- | Field of points of SECP112R2 curve.
type Fq = PrimeField 0xdb7c2abf62e35e668076bead208b

-- | Field of coefficients of SECP112R2 curve.
type Fr = PrimeField 0x36df0aafd8b8d7597ca10520d04b

-- | SECP112R2 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP112R2 Fq => WCurve c SECP112R2 Fq where
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

-- | Affine SECP112R2 curve point.
type PA = WAPoint SECP112R2 Fq

-- | Affine SECP112R2 curve is a Weierstrass affine curve.
instance WACurve SECP112R2 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective SECP112R2 point.
type PP = WPPoint SECP112R2 Fq

-- | Projective SECP112R2 curve is a Weierstrass projective curve.
instance WPCurve SECP112R2 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP112R2 curve.
_a :: Fq
_a = 0x6127c24c05f38a0aaaf65c0ef02c
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP112R2 curve.
_b :: Fq
_b = 0x51def1815db5ed74fcc34c85d709
{-# INLINE _b #-}

-- | Cofactor of SECP112R2 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Characteristic of SECP112R2 curve.
_q :: Integer
_q = 0xdb7c2abf62e35e668076bead208b
{-# INLINE _q #-}

-- | Order of SECP112R2 curve.
_r :: Integer
_r = 0x36df0aafd8b8d7597ca10520d04b
{-# INLINE _r #-}

-- | Coordinate @X@ of SECP112R2 curve.
_x :: Fq
_x = 0x4ba30ab5e892b4e1649dd0928643
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP112R2 curve.
_y :: Fq
_y = 0xadcd46f5882e3747def36e956e97
{-# INLINE _y #-}

-- | Affine generator of SECP112R2 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Projective generator of SECP112R2 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
