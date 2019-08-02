module Curve.Weierstrass.SECP256K1
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

-- | SECP256K1 curve.
data SECP256K1

-- | Field of points of SECP256K1 curve.
type Fq = PrimeField 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f

-- | Field of coefficients of SECP256K1 curve.
type Fr = PrimeField 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

-- | SECP256K1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP256K1 Fq => WCurve c SECP256K1 Fq where
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

-- | Affine SECP256K1 curve point.
type PA = WAPoint SECP256K1 Fq

-- | Affine SECP256K1 curve is a Weierstrass affine curve.
instance WACurve SECP256K1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian SECP256K1 point.
type PJ = WJPoint SECP256K1 Fq

-- | Jacobian SECP256K1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP256K1 Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective SECP256K1 point.
type PP = WPPoint SECP256K1 Fq

-- | Projective SECP256K1 curve is a Weierstrass projective curve.
instance WPCurve SECP256K1 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP256K1 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP256K1 curve.
_b :: Fq
_b = 0x7
{-# INLINE _b #-}

-- | Cofactor of SECP256K1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP256K1 curve.
_q :: Integer
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
{-# INLINE _q #-}

-- | Order of SECP256K1 curve.
_r :: Integer
_r = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
{-# INLINE _r #-}

-- | Coordinate @X@ of SECP256K1 curve.
_x :: Fq
_x = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP256K1 curve.
_y :: Fq
_y = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
{-# INLINE _y #-}

-- | Generator of affine SECP256K1 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of Jacobian SECP256K1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINE gJ #-}

-- | Generator of projective SECP256K1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
