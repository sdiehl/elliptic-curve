module Curve.Weierstrass.SECP160R1
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

-- | SECP160R1 curve.
data SECP160R1

-- | Field of points of SECP160R1 curve.
type Fq = PrimeField 0xffffffffffffffffffffffffffffffff7fffffff

-- | Field of coefficients of SECP160R1 curve.
type Fr = PrimeField 0x100000000000000000001f4c8f927aed3ca752257

-- | SECP160R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP160R1 Fq => WCurve c SECP160R1 Fq where
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

-- | Affine SECP160R1 curve point.
type PA = WAPoint SECP160R1 Fq

-- | Affine SECP160R1 curve is a Weierstrass affine curve.
instance WACurve SECP160R1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian SECP160R1 point.
type PJ = WJPoint SECP160R1 Fq

-- | Jacobian SECP160R1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP160R1 Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective SECP160R1 point.
type PP = WPPoint SECP160R1 Fq

-- | Projective SECP160R1 curve is a Weierstrass projective curve.
instance WPCurve SECP160R1 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP160R1 curve.
_a :: Fq
_a = 0xffffffffffffffffffffffffffffffff7ffffffc
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP160R1 curve.
_b :: Fq
_b = 0x1c97befc54bd7a8b65acf89f81d4d4adc565fa45
{-# INLINE _b #-}

-- | Cofactor of SECP160R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP160R1 curve.
_q :: Integer
_q = 0xffffffffffffffffffffffffffffffff7fffffff
{-# INLINE _q #-}

-- | Order of SECP160R1 curve.
_r :: Integer
_r = 0x100000000000000000001f4c8f927aed3ca752257
{-# INLINE _r #-}

-- | Coordinate @X@ of SECP160R1 curve.
_x :: Fq
_x = 0x4a96b5688ef573284664698968c38bb913cbfc82
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP160R1 curve.
_y :: Fq
_y = 0x23a628553168947d59dcc912042351377ac5fb32
{-# INLINE _y #-}

-- | Generator of affine SECP160R1 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of Jacobian SECP160R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINE gJ #-}

-- | Generator of projective SECP160R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
