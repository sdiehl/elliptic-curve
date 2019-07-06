module Curve.ShortWeierstrass
  -- | Types
  ( Point(..)
  , SWCurve(..)
  , SWPoint
  ) where

import Protolude

import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..))

import Curve (Curve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Short Weierstrass representation
data SW

-- | Short Weierstrass points
type SWPoint = Point SW

-- | Short Weierstrass curves @Y^2 = X^3 + AX + B@
class Curve SW c k => SWCurve c k where
  a_ :: (c, k) -> k -- ^ A
  b_ :: (c, k) -> k -- ^ B
  g_ :: SWPoint c k -- ^ generator

-- | Short Weierstrass curves are arbitrary
instance SWCurve c k => Arbitrary (Point SW c k) where
  arbitrary = return g_

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Short Weierstrass curves are elliptic curves
instance (GaloisField k, SWCurve c k) => Curve SW c k where

  data instance Point SW c k = A k k -- ^ Affine point
                             | O     -- ^ Infinite point
    deriving (Eq, Generic, NFData, Show)

  id = O
  {-# INLINE id #-}

  inv O       = O
  inv (A x y) = A x (-y)
  {-# INLINE inv #-}

  add p O          = p
  add O q          = q
  add p@(A x1 y1) (A x2 y2)
    | x1 /= x2     = A x3 y3
    | y1 + y2 == 0 = O
    | otherwise    = double p
    where
      l  = (y1 - y2) / (x1 - x2)
      x3 = l * l - x1 - x2
      y3 = l * (x1 - x3) - y1
  {-# INLINE add #-}

  double O       = O
  double (A x y) = A x' y'
    where
      l  = (3 * x * x + a_ (witness :: (c, k))) / (2 * y)
      x' = l * l - 2 * x
      y' = l * (x - x') - y
  {-# INLINE double #-}

  def O       = True
  def (A x y) = y * y == x * (x * x + a) + b
    where
      a = a_ (witness :: (c, k))
      b = b_ (witness :: (c, k))
  {-# INLINE def #-}
