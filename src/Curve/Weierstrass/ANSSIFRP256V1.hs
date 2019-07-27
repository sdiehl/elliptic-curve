module Curve.Weierstrass.ANSSIFRP256V1
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

-- | ANSSIFRP256V1 curve.
data ANSSIFRP256V1

-- | Field of ANSSIFRP256V1 curve.
type Fp = PrimeField 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c03

-- | ANSSIFRP256V1 curve is a Weierstrass curve.
instance WCurve ANSSIFRP256V1 Fp where
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

-- | Point of ANSSIFRP256V1 curve.
type P = WPoint ANSSIFRP256V1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of ANSSIFRP256V1 curve.
_a :: Fp
_a = 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c00
{-# INLINE _a #-}

-- | Coefficient @B@ of ANSSIFRP256V1 curve.
_b :: Fp
_b = 0xee353fca5428a9300d4aba754a44c00fdfec0c9ae4b1a1803075ed967b7bb73f
{-# INLINE _b #-}

-- | Generator of ANSSIFRP256V1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of ANSSIFRP256V1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of ANSSIFRP256V1 curve.
_n :: Integer
_n = 0xf1fd178c0b3ad58f10126de8ce42435b53dc67e140d2bf941ffdd459c6d655e1
{-# INLINE _n #-}

-- | Characteristic of ANSSIFRP256V1 curve.
_p :: Integer
_p = 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c03
{-# INLINE _p #-}

-- | Coordinate @X@ of ANSSIFRP256V1 curve.
_x :: Fp
_x = 0xb6b3d4c356c139eb31183d4749d423958c27d2dcaf98b70164c97a2dd98f5cff
{-# INLINE _x #-}

-- | Coordinate @Y@ of ANSSIFRP256V1 curve.
_y :: Fp
_y = 0x6142e0f7c8b204911f9271f0f3ecef8c2701c307e8e4c9e183115a1554062cfb
{-# INLINE _y #-}
