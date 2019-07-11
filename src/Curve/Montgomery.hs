module Curve.Montgomery
  -- | Types
  ( Point(..)
  , MCurve(..)
  , MPoint
  ) where

import Protolude

import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..))

import Curve (Curve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Montgomery representation
data M

-- | Montgomery points
type MPoint = Point M

-- | Montgomery curves @BY^2 = X^3 + AX^2 + X@
class Curve M c k => MCurve c k where
  a_ :: c -> k     -- ^ A
  b_ :: c -> k     -- ^ B
  g_ :: MPoint c k -- ^ generator

-- | Montgomery curves are arbitrary
instance MCurve c k => Arbitrary (Point M c k) where
  arbitrary = return g_

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Montgomery curves are elliptic curves
instance (GaloisField k, MCurve c k) => Curve M c k where

  data instance Point M c k = A k k -- ^ Affine point
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
      a  = a_ (witness :: c)
      b  = b_ (witness :: c)
      l  = (y2 - y1) / (x2 - x1)
      x3 = b * l * l - a - x1 - x2
      y3 = l * (x1 - x3) - y1
  {-# INLINE add #-}

  double O       = O
  double (A x y) = A x' y'
    where
      a  = a_ (witness :: c)
      b  = b_ (witness :: c)
      l  = (x * (3 * x + 2 * a) + 1) / (2 * b * y)
      x' = b * l * l - a - 2 * x
      y' = l * (x - x') - y
  {-# INLINE double #-}

  def O       = True
  def (A x y) = b * y * y == (((x + a) * x) + 1) * x
    where
      a = a_ (witness :: c)
      b = b_ (witness :: c)
  {-# INLINE def #-}

  disc _ = b * (a ^ 2 - 4)
    where
      a = a_ (witness :: c)
      b = b_ (witness :: c)
  {-# INLINE disc #-}
