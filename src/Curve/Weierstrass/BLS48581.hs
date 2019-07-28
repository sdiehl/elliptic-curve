module Curve.Weierstrass.BLS48581
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

-- | BLS48581 curve.
data BLS48581

-- | Field of points of BLS48581 curve.
type Fq = PrimeField 0x1280f73ff3476f313824e31d47012a0056e84f8d122131bb3be6c0f1f3975444a48ae43af6e082acd9cd30394f4736daf68367a5513170ee0a578fdf721a4a48ac3edc154e6565912b

-- | Field of coefficients of BLS48581 curve.
type Fr = PrimeField 0x2386f8a925e2885e233a9ccc1615c0d6c635387a3f0b3cbe003fad6bc972c2e6e741969d34c4c92016a85c7cd0562303c4ccbe599467c24da118a5fe6fcd671c01

-- | BLS48581 curve is a Weierstrass curve.
instance WCurve BLS48581 Fq where
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

-- | Point of BLS48581 curve.
type P = WPoint BLS48581 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BLS48581 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BLS48581 curve.
_b :: Fq
_b = 0x1
{-# INLINE _b #-}

-- | Generator of BLS48581 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of BLS48581 curve.
_h :: Integer
_h = 0x85555841aaaec4ac
{-# INLINE _h #-}

-- | Characteristic of BLS48581 curve.
_q :: Integer
_q = 0x1280f73ff3476f313824e31d47012a0056e84f8d122131bb3be6c0f1f3975444a48ae43af6e082acd9cd30394f4736daf68367a5513170ee0a578fdf721a4a48ac3edc154e6565912b
{-# INLINE _q #-}

-- | Order of BLS48581 curve.
_r :: Integer
_r = 0x2386f8a925e2885e233a9ccc1615c0d6c635387a3f0b3cbe003fad6bc972c2e6e741969d34c4c92016a85c7cd0562303c4ccbe599467c24da118a5fe6fcd671c01
{-# INLINE _r #-}

-- | Coordinate @X@ of BLS48581 curve.
_x :: Fq
_x = 0x2af59b7ac340f2baf2b73df1e93f860de3f257e0e86868cf61abdbaedffb9f7544550546a9df6f9645847665d859236ebdbc57db368b11786cb74da5d3a1e6d8c3bce8732315af640
{-# INLINE _x #-}

-- | Coordinate @Y@ of BLS48581 curve.
_y :: Fq
_y = 0xcefda44f6531f91f86b3a2d1fb398a488a553c9efeb8a52e991279dd41b720ef7bb7beffb98aee53e80f678584c3ef22f487f77c2876d1b2e35f37aef7b926b576dbb5de3e2587a70
{-# INLINE _y #-}
