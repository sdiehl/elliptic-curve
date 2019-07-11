module Curve.Edwards
  -- | Types
  ( ECurve(..)
  , EPoint
  , Point(..)
  ) where

import Protolude

import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..))

import Curve (Curve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Edwards representation
data E

-- | Edwards points
type EPoint = Point E

-- | Edwards curves @AX^2 + Y^2 = 1 + DX^2Y^2@
class Curve E c k => ECurve c k where
  a_ :: c -> k     -- ^ A
  d_ :: c -> k     -- ^ D
  g_ :: EPoint c k -- ^ generator

-- | Edwards curves are arbitrary
instance ECurve c k => Arbitrary (Point E c k) where
  arbitrary = return g_

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Edwards curves are elliptic curves
instance (GaloisField k, ECurve c k) => Curve E c k where

  data instance Point E c k = A k k -- ^ Affine point
    deriving (Eq, Generic, NFData, Show)

  id = A 0 1
  {-# INLINE id #-}

  inv (A x y) = A (-x) y
  {-# INLINE inv #-}

  add (A x1 y1) (A x2 y2) = A x3 y3
    where
      a    = a_ (witness :: c)
      d    = d_ (witness :: c)
      x1x2 = x1 * x2
      y1y2 = y1 * y2
      x1y2 = x1 * y2
      x2y1 = x2 * y1
      dxy  = d * x1x2 * y1y2
      x3   = (x1y2 + x2y1) / (1 + dxy)
      y3   = (y1y2 - a * x1x2) / (1 - dxy)
  {-# INLINE add #-}

  def (A x y) = a * xx + yy == 1 + d * xx * yy
    where
      a  = a_ (witness :: c)
      d  = d_ (witness :: c)
      xx = x * x
      yy = y * y
  {-# INLINE def #-}

  disc _ = d * (1 - d)
    where
      d = d_ (witness :: c)
  {-# INLINE disc #-}
