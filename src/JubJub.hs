module JubJub
  ( Fq
  , Point
  , _d
  , _h
  , _r
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve (Curve(..))

-- | Curve
data JubJub

-- | Field of curve
type Fq = PrimeField 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

-- | Point of curve
type Point = P JubJub Fq

-- | Coefficient of curve
_d :: Fq
_d = -(10240 / 10241)
{-# INLINE _d #-}

-- | Cofactor of curve
_h :: Integer
_h = 8
{-# INLINE _h #-}

-- | Order of curve
_r :: Integer
_r = 0x0e7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
{-# INLINE _r #-}

-- | JubJub is a curve over Fq
instance Curve JubJub Fq where
  data P JubJub Fq = A Fq Fq
    deriving (Eq, Show, Generic, NFData)

  id = A 0 1
  {-# INLINE id #-}

  inv (A x y) = A (-x) y
  {-# INLINE inv #-}

  add p@(A x1 y1) q@(A x2 y2)
    | p == q    = double p
    | otherwise = A ((x1y2 + x2y1) / (1 + dxy)) ((y1y2 + x1x2) / (1 - dxy))
    where
      x1x2 = x1 * x2
      y1y2 = y1 * y2
      x1y2 = x1 * y2
      x2y1 = x2 * y1
      dxy  = _d * x1x2 * y1y2
  {-# INLINE add #-}

  double (A x y) = A ((xy + xy) / (1 + dxy)) ((yy + xx) / (1 - dxy))
    where
      xx  = x * x
      xy  = x * y
      yy  = y * y
      dxy = _d * xx * yy
  {-# INLINE double #-}

  def (A x y) = -xx + yy == 1 + _d * xx * yy
    where
      xx = x * x
      yy = y * y
  {-# INLINE def #-}
