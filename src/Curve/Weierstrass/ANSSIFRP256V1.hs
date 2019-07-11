module Curve.Weierstrass.ANSSIFRP256V1
  -- | Types
  ( Fp
  , P
  -- | Parameters
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | ANSSI-FRP256V1 curve
data ANSSIFRP256V1

-- | Field of ANSSI-FRP256V1 curve
type Fp = PrimeField 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c03

-- | ANSSI-FRP256V1 curve is a Weierstrass curve
instance WCurve ANSSIFRP256V1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of ANSSI-FRP256V1 curve
type P = WPoint ANSSIFRP256V1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of ANSSI-FRP256V1 curve
_a :: Fp
_a = 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c00
{-# INLINE _a #-}

-- | Coefficient @B@ of ANSSI-FRP256V1 curve
_b :: Fp
_b = 0xee353fca5428a9300d4aba754a44c00fdfec0c9ae4b1a1803075ed967b7bb73f
{-# INLINE _b #-}

-- | Generator of ANSSI-FRP256V1 curve
_g :: P
_g = A
     0xb6b3d4c356c139eb31183d4749d423958c27d2dcaf98b70164c97a2dd98f5cff
     0x6142e0f7c8b204911f9271f0f3ecef8c2701c307e8e4c9e183115a1554062cfb
{-# INLINE _g #-}

-- | Cofactor of ANSSI-FRP256V1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of ANSSI-FRP256V1 curve
_n :: Integer
_n = notImplemented
{-# INLINE _n #-}

-- | Characteristic of ANSSI-FRP256V1 curve
_p :: Integer
_p = 0xf1fd178c0b3ad58f10126de8ce42435b3961adbcabc8ca6de8fcf353d86e9c03
{-# INLINE _p #-}
