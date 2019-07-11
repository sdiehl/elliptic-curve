module Curve.Weierstrass
  -- | Types
  ( Point(..)
  , WCurve(..)
  , WPoint
  ) where

import Protolude

import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..))

import Curve (Curve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Weierstrass representation
data W

-- | Weierstrass points
type WPoint = Point W

-- | Weierstrass curves @Y^2 = X^3 + AX + B@
class Curve W c k => WCurve c k where
  a_ :: c -> k     -- ^ A
  b_ :: c -> k     -- ^ B
  g_ :: WPoint c k -- ^ generator

-- | Weierstrass curves are arbitrary
instance WCurve c k => Arbitrary (Point W c k) where
  arbitrary = return g_

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Weierstrass curves are elliptic curves
instance (GaloisField k, WCurve c k) => Curve W c k where

  data instance Point W c k = A k k -- ^ Affine point
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
      l  = (3 * x * x + a_ (witness :: c)) / (2 * y)
      x' = l * l - 2 * x
      y' = l * (x - x') - y
  {-# INLINE double #-}

  def O       = True
  def (A x y) = y * y == x * (x * x + a) + b
    where
      a = a_ (witness :: c)
      b = b_ (witness :: c)
  {-# INLINE def #-}
