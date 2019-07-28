module Curve.Weierstrass.SECP112R2
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
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

import Curve (Curve(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Group (Group(..))

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
instance WCurve SECP112R2 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
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

-- | Point of SECP112R2 curve.
type P = WPoint SECP112R2 Fq

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

-- | Generator of SECP112R2 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

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
