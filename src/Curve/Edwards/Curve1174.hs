module Curve.Edwards.Curve1174
  ( Curve(..)
  , EPoint
  , ECurve(..)
  , Fq
  , Fr
  , Group(..)
  , P
  , Point(..)
  , _a
  , _d
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
import Curve.Edwards (ECurve(..), EPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Curve1174 curve.
data Curve1174

-- | Field of points of Curve1174 curve.
type Fq = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7

-- | Field of coefficients of Curve1174 curve.
type Fr = PrimeField 0x1fffffffffffffffffffffffffffffff77965c4dfd307348944d45fd166c971

-- | Curve1174 curve is an Edwards curve.
instance ECurve Curve1174 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
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

-- | Point of Curve1174 curve.
type P = EPoint Curve1174 Fq

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Curve1174 curve.
_a :: Fq
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @B@ of Curve1174 curve.
_d :: Fq
_d = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffb61
{-# INLINE _d #-}

-- | Generator of Curve1174 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of Curve1174 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Characteristic of Curve1174 curve.
_q :: Integer
_q = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7
{-# INLINE _q #-}

-- | Order of Curve1174 curve.
_r :: Integer
_r = 0x1fffffffffffffffffffffffffffffff77965c4dfd307348944d45fd166c971
{-# INLINE _r #-}

-- | Coordinate @X@ of Curve1174 curve.
_x :: Fq
_x = 0x37fbb0cea308c479343aee7c029a190c021d96a492ecd6516123f27bce29eda
{-# INLINE _x #-}

-- | Coordinate @Y@ of Curve1174 curve.
_y :: Fq
_y = 0x6b72f82d47fb7cc6656841169840e0c4fe2dee2af3f976ba4ccb1bf9b46360e
{-# INLINE _y #-}
