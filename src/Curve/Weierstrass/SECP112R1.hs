module Curve.Weierstrass.SECP112R1
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

-- | SECP112R1 curve.
data SECP112R1

-- | Field of points of SECP112R1 curve.
type Fq = PrimeField 0xdb7c2abf62e35e668076bead208b

-- | Field of coefficients of SECP112R1 curve.
type Fr = PrimeField 0xdb7c2abf62e35e7628dfac6561c5

-- | SECP112R1 curve is a Weierstrass curve.
instance WCurve SECP112R1 Fq where
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

-- | Point of SECP112R1 curve.
type P = WPoint SECP112R1 Fq

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

-- | Generator of SECP112R1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

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
