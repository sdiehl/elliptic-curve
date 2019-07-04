module Curve.TwistedEdwards
  -- | Types
  ( Point(..)
  , TECurve(..)
  , TEPoint
  ) where

import Protolude

import GaloisField (GaloisField(..))

import Curve (Curve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Twisted Edwards representation
data TE

-- | Twisted Edwards curves @aX^2 + Y^2 = 1 + dX^2Y^2@
class Curve TE c k => TECurve c k where
  a :: c -> k -- ^ a
  d :: c -> k -- ^ d

-- | Twisted Edwards points
type TEPoint = Point TE

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Twisted Edwards curves are elliptic curves
instance (GaloisField k, TECurve c k) => Curve TE c k where

  data instance Point TE c k = A k k -- ^ Affine point
    deriving (Eq, Generic, NFData, Show)

  id = A 0 1
  {-# INLINE id #-}

  inv (A x y) = A (-x) y
  {-# INLINE inv #-}

  add p@(A x1 y1) q@(A x2 y2)
    | p == q    = double p
    | otherwise = A ((x1y2 + x2y1) / (1 + dxy)) ((y1y2 - a_ * x1x2) / (1 - dxy))
    where
      a_   = a (witness :: c)
      d_   = d (witness :: c)
      x1x2 = x1 * x2
      y1y2 = y1 * y2
      x1y2 = x1 * y2
      x2y1 = x2 * y1
      dxy  = d_ * x1x2 * y1y2
  {-# INLINE add #-}

  double (A x y) = A ((xy + xy) / (1 + dxy)) ((yy - a_ * xx) / (1 - dxy))
    where
      a_  = a (witness :: c)
      d_  = d (witness :: c)
      xx  = x * x
      xy  = x * y
      yy  = y * y
      dxy = d_ * xx * yy
  {-# INLINE double #-}

  def (A x y) = a_ * xx + yy == 1 + d_ * xx * yy
    where
      a_ = a (witness :: c)
      d_ = d (witness :: c)
      xx = x * x
      yy = y * y
  {-# INLINE def #-}
