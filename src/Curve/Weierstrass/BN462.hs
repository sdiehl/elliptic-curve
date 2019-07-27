module Curve.Weierstrass.BN462
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

import Curve (Curve(..), Group(..))
import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN462 curve.
data BN462

-- | Field of BN462 curve.
type Fp = PrimeField 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908f41c8020ffffffffff6ff66fc6ff687f640000000002401b00840138013

-- | BN462 curve is a Weierstrass curve.
instance WCurve BN462 Fp where
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

-- | Point of BN462 curve.
type P = WPoint BN462 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN462 curve.
_a :: Fp
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN462 curve.
_b :: Fp
_b = 0x5
{-# INLINE _b #-}

-- | Generator of BN462 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BN462 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of BN462 curve.
_n :: Integer
_n = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908ee1c201f7fffffffff6ff66fc7bf717f7c0000000002401b007e010800d
{-# INLINE _n #-}

-- | Characteristic of BN462 curve.
_p :: Integer
_p = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908f41c8020ffffffffff6ff66fc6ff687f640000000002401b00840138013
{-# INLINE _p #-}

-- | Coordinate @X@ of BN462 curve.
_x :: Fp
_x = 0x21a6d67ef250191fadba34a0a30160b9ac9264b6f95f63b3edbec3cf4b2e689db1bbb4e69a416a0b1e79239c0372e5cd70113c98d91f36b6980d
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN462 curve.
_y :: Fp
_y = 0x118ea0460f7f7abb82b33676a7432a490eeda842cccfa7d788c659650426e6af77df11b8ae40eb80f475432c66600622ecaa8a5734d36fb03de
{-# INLINE _y #-}
