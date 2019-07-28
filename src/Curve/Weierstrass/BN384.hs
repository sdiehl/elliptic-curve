module Curve.Weierstrass.BN384
  ( Curve(..)
  , Fp
  , Group(..)
  , P
  , Point(..)
  , WPoint
  , WCurve(..)
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

import Curve (Curve(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN384 curve.
data BN384

-- | Field of BN384 curve.
type Fp = PrimeField 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca024c8fbe29531892c79534f9d306328261550a7cabd7cccd10b

-- | BN384 curve is a Weierstrass curve.
instance WCurve BN384 Fp where
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
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Point of BN384 curve.
type P = WPoint BN384 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN384 curve.
_a :: Fp
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN384 curve.
_b :: Fp
_b = 0x3
{-# INLINE _b #-}

-- | Generator of BN384 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BN384 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of BN384 curve.
_n :: Integer
_n = 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca023c8fbe29531892c795356487d8ac63e4f4db17384341a5775
{-# INLINE _n #-}

-- | Characteristic of BN384 curve.
_p :: Integer
_p = 0xfffffffffffffffffff2a96823d5920d2a127e3f6fbca024c8fbe29531892c79534f9d306328261550a7cabd7cccd10b
{-# INLINE _p #-}

-- | Coordinate @X@ of BN384 curve.
_x :: Fp
_x = 0x1
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN384 curve.
_y :: Fp
_y = 0x2
{-# INLINE _y #-}
