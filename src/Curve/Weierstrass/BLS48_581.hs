module Curve.Weierstrass.BLS48_581
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

-- | BLS48-581 curve
data BLS48_581

-- | Field of BLS48-581 curve
type Fp = PrimeField 0x1280f73ff3476f313824e31d47012a0056e84f8d122131bb3be6c0f1f3975444a48ae43af6e082acd9cd30394f4736daf68367a5513170ee0a578fdf721a4a48ac3edc154e6565912b

-- | BLS48-581 curve is a Weierstrass curve
instance WCurve BLS48_581 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of BLS48-581 curve
type P = WPoint BLS48_581 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BLS48-581 curve
_a :: Fp
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of BLS48-581 curve
_b :: Fp
_b = 1
{-# INLINE _b #-}

-- | Generator of BLS48-581 curve
_g :: P
_g = A
     5
     0x491acfa2307425af23c3444bb9f7c38b86fe62a4105f1a06bac418fb4244afb7b6b932b9a4a3c048637613a50e88b86e9e37a154f077398b0d26f51ce737e2e1e768d5b0dc461d83a
{-# INLINE _g #-}

-- | Cofactor of BLS48-581 curve
_h :: Integer
_h = 0x85555841aaaec4ac
{-# INLINE _h #-}

-- | Order of BLS48-581 curve
_n :: Integer
_n = 0x2386f8a925e2885e233a9ccc1615c0d6c635387a3f0b3cbe003fad6bc972c2e6e741969d34c4c92016a85c7cd0562303c4ccbe599467c24da118a5fe6fcd671c01
{-# INLINE _n #-}

-- | Characteristic of BLS48-581 curve
_p :: Integer
_p = 0x1280f73ff3476f313824e31d47012a0056e84f8d122131bb3be6c0f1f3975444a48ae43af6e082acd9cd30394f4736daf68367a5513170ee0a578fdf721a4a48ac3edc154e6565912b
{-# INLINE _p #-}
