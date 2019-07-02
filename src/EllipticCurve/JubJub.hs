module EllipticCurve.JubJub
  ( Fq
  , Point
  , P(..)
  , _d
  , _h
  , _q
  , _r
  ) where

import Protolude

import PrimeField (PrimeField)

import EllipticCurve (EC(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Field of JubJub curve
type Fq = PrimeField 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

-- | JubJub curve @-x^2+y^2=1+dx^2y^2@
data JubJub

-- | Point of JubJub curve
type Point = P JubJub Fq

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | JubJub curve is an elliptic curve over @Fq@
instance EC JubJub Fq where
  data P JubJub Fq = A Fq Fq -- ^ Affine point
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

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient of JubJub curve
_d :: Fq
_d = -(10240 / 10241)
{-# INLINE _d #-}

-- | Cofactor of JubJub curve
_h :: Integer
_h = 8
{-# INLINE _h #-}

-- | Characteristic of JubJub curve
_q :: Fq
_q = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
{-# INLINE _q #-}

-- | Order of JubJub curve
_r :: Integer
_r = 0x0e7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7
{-# INLINE _r #-}
