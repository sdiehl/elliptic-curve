module Curve.Weierstrass.SECP112R1
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

-- | SECP112R1 curve.
data SECP112R1

-- | Field of points of SECP112R1 curve.
type Fq = PrimeField 0xdb7c2abf62e35e668076bead208b

-- | Field of coefficients of SECP112R1 curve.
type Fr = PrimeField 0xdb7c2abf62e35e7628dfac6561c5

-- | SECP112R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP112R1 Fq => WCurve c SECP112R1 Fq where
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

-- | Affine SECP112R1 curve point.
type PA = WAPoint SECP112R1 Fq

-- | Affine SECP112R1 curve is a Weierstrass affine curve.
instance WACurve SECP112R1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian SECP112R1 point.
type PJ = WJPoint SECP112R1 Fq

-- | Jacobian SECP112R1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP112R1 Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective SECP112R1 point.
type PP = WPPoint SECP112R1 Fq

-- | Projective SECP112R1 curve is a Weierstrass projective curve.
instance WPCurve SECP112R1 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP112R1 curve.
_a :: Fq
_a = 0xdb7c2abf62e35e668076bead2088
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP112R1 curve.
_b :: Fq
_b = 0x659ef8ba043916eede8911702b22
{-# INLINE _b #-}

-- | Cofactor of SECP112R1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of SECP112R1 curve.
_q :: Integer
_q = 0xdb7c2abf62e35e668076bead208b
{-# INLINE _q #-}

-- | Order of SECP112R1 curve.
_r :: Integer
_r = 0xdb7c2abf62e35e7628dfac6561c5
{-# INLINE _r #-}

-- | Coordinate @X@ of SECP112R1 curve.
_x :: Fq
_x = 0x9487239995a5ee76b55f9c2f098
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP112R1 curve.
_y :: Fq
_y = 0xa89ce5af8724c0a23e0e0ff77500
{-# INLINE _y #-}

-- | Generator of affine SECP112R1 curve.
gA :: PA
gA = A _x _y
{-# INLINE gA #-}

-- | Generator of Jacobian SECP112R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINE gJ #-}

-- | Generator of projective SECP112R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINE gP #-}
