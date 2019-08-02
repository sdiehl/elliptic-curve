module Curve.Weierstrass.SECP112R2
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

-- | Jacobian SECP112R2 point.
type PJ = WJPoint SECP112R2 Fq

-- | Jacobian SECP112R2 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP112R2 Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

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

-- | Generator of affine SECP112R2 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of Jacobian SECP112R2 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINE gJ #-}

-- | Generator of projective SECP112R2 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
