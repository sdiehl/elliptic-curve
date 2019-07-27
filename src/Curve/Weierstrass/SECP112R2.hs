module Curve.Weierstrass.SECP112R2
  ( Fp
  , P
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP112R2 curve
data SECP112R2

-- | Field of SECP112R2 curve
type Fp = PrimeField 0xdb7c2abf62e35e668076bead208b

-- | SECP112R2 curve is a Weierstrass curve
instance WCurve SECP112R2 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}

-- | Point of SECP112R2 curve
type P = WPoint SECP112R2 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP112R2 curve
_a :: Fp
_a = 0x6127c24c05f38a0aaaf65c0ef02c
{-# INLINE _a #-}

-- | Coefficient @B@ of SECP112R2 curve
_b :: Fp
_b = 0x51def1815db5ed74fcc34c85d709
{-# INLINE _b #-}

-- | Generator of SECP112R2 curve
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECP112R2 curve
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Order of SECP112R2 curve
_n :: Integer
_n = 0x36df0aafd8b8d7597ca10520d04b
{-# INLINE _n #-}

-- | Characteristic of SECP112R2 curve
_p :: Integer
_p = 0xdb7c2abf62e35e668076bead208b
{-# INLINE _p #-}

-- | Coordinate @X@ of SECP112R2 curve
_x :: Fp
_x = 0x4ba30ab5e892b4e1649dd0928643
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECP112R2 curve
_y :: Fp
_y = 0xadcd46f5882e3747def36e956e97
{-# INLINE _y #-}
