module Curve.Weierstrass.BrainpoolP224T1
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

-- | BrainpoolP224T1 curve.
data BrainpoolP224T1

-- | Field of BrainpoolP224T1 curve.
type Fp = PrimeField 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff

-- | BrainpoolP224T1 curve is a Weierstrass curve.
instance WCurve BrainpoolP224T1 Fp where
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

-- | Point of BrainpoolP224T1 curve.
type P = WPoint BrainpoolP224T1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP224T1 curve.
_a :: Fp
_a = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0fc
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP224T1 curve.
_b :: Fp
_b = 0x4b337d934104cd7bef271bf60ced1ed20da14c08b3bb64f18a60888d
{-# INLINE _b #-}

-- | Generator of BrainpoolP224T1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BrainpoolP224T1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Order of BrainpoolP224T1 curve.
_n :: Integer
_n = 0xd7c134aa264366862a18302575d0fb98d116bc4b6ddebca3a5a7939f
{-# INLINE _n #-}

-- | Characteristic of BrainpoolP224T1 curve.
_p :: Integer
_p = 0xd7c134aa264366862a18302575d1d787b09f075797da89f57ec8c0ff
{-# INLINE _p #-}

-- | Coordinate @X@ of BrainpoolP224T1 curve.
_x :: Fp
_x = 0x6ab1e344ce25ff3896424e7ffe14762ecb49f8928ac0c76029b4d580
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP224T1 curve.
_y :: Fp
_y = 0x374e9f5143e568cd23f3f4d7c0d4b1e41c8cc0d1c6abd5f1a46db4c
{-# INLINE _y #-}
