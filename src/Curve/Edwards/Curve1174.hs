module Curve.Edwards.Curve1174
  ( Fp
  , P
  , _a
  , _d
  , _g
  , _h
  , _n
  , _p
  , _x
  , _y
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Edwards (ECurve(..), EPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Curve1174 curve
data Curve1174

-- | Field of Curve1174 curve
type Fp = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7

-- | Curve1174 curve is an Edwards curve
instance ECurve Curve1174 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  d_ = const _d
  {-# INLINE d_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}

-- | Point of Curve1174 curve
type P = EPoint Curve1174 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Curve1174 curve
_a :: Fp
_a = 0x1
{-# INLINE _a #-}

-- | Coefficient @B@ of Curve1174 curve
_d :: Fp
_d = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffb61
{-# INLINE _d #-}

-- | Generator of Curve1174 curve
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of Curve1174 curve
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Order of Curve1174 curve
_n :: Integer
_n = 0x1fffffffffffffffffffffffffffffff77965c4dfd307348944d45fd166c971
{-# INLINE _n #-}

-- | Characteristic of Curve1174 curve
_p :: Integer
_p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7
{-# INLINE _p #-}

-- | Coordinate @X@ of Curve1174 curve
_x :: Fp
_x = 0x37fbb0cea308c479343aee7c029a190c021d96a492ecd6516123f27bce29eda
{-# INLINE _x #-}

-- | Coordinate @Y@ of Curve1174 curve
_y :: Fp
_y = 0x6b72f82d47fb7cc6656841169840e0c4fe2dee2af3f976ba4ccb1bf9b46360e
{-# INLINE _y #-}
