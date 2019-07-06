module Curve.TwistedEdwards
  -- | Types
  ( Point(..)
  , TECurve(..)
  , TEPoint
  ) where

import Protolude

import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..))

import Curve (Curve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Twisted Edwards representation
data TE

-- | Twisted Edwards points
type TEPoint = Point TE

-- | Twisted Edwards curves @AX^2 + Y^2 = 1 + DX^2Y^2@
class Curve TE c k => TECurve c k where
  a_ :: (c, k) -> k -- ^ A
  d_ :: (c, k) -> k -- ^ D
  g_ :: TEPoint c k -- ^ generator

-- | Twisted Edwards curves are arbitrary
instance TECurve c k => Arbitrary (Point TE c k) where
  arbitrary = return g_

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
    | otherwise = A ((x1y2 + x2y1) / (1 + dxy)) ((y1y2 - a * x1x2) / (1 - dxy))
    where
      a    = a_ (witness :: (c, k))
      d    = d_ (witness :: (c, k))
      x1x2 = x1 * x2
      y1y2 = y1 * y2
      x1y2 = x1 * y2
      x2y1 = x2 * y1
      dxy  = d * x1x2 * y1y2
  {-# INLINE add #-}

  double (A x y) = A ((xy + xy) / (1 + dxy)) ((yy - a * xx) / (1 - dxy))
    where
      a   = a_ (witness :: (c, k))
      d   = d_ (witness :: (c, k))
      xx  = x * x
      xy  = x * y
      yy  = y * y
      dxy = d * xx * yy
  {-# INLINE double #-}

  def (A x y) = a * xx + yy == 1 + d * xx * yy
    where
      a  = a_ (witness :: (c, k))
      d  = d_ (witness :: (c, k))
      xx = x * x
      yy = y * y
  {-# INLINE def #-}
