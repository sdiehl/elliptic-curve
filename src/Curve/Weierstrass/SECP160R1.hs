module Curve.Weierstrass.SECP160R1
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

-- | SECP160R1 curve.
data SECP160R1

-- | Field of points of SECP160R1 curve.
type Fq = PrimeField 0xffffffffffffffffffffffffffffffff7fffffff

-- | Field of coefficients of SECP160R1 curve.
type Fr = PrimeField 0x100000000000000000001f4c8f927aed3ca752257

-- | SECP160R1 curve is a Weierstrass curve.
instance WCurve SECP160R1 Fq where
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

-- | Point of SECP160R1 curve.
type P = WPoint SECP160R1 Fq

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

-- | Generator of SECP160R1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

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
