module Curve.Binary
  -- | Types
  ( BCurve(..)
  , BPoint
  , Point(..)
  ) where

import Protolude

import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..))

import Curve (Curve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Binary curve representation.
data B

-- | Binary curve points.
type BPoint = Point B

-- | Binary curves @y^2 + xy = x^3 + Ax^2 + B@.
class Curve B c k => BCurve c k where
  a_ :: c -> k     -- ^ Coefficient @A@.
  b_ :: c -> k     -- ^ Coefficient @B@.
  g_ :: BPoint c k -- ^ Curve generator.

-- Binary curves are arbitrary.
instance BCurve c k => Arbitrary (Point B c k) where
  arbitrary = return g_
  -- arbitrary = suchThatMap arbitrary point

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- Binary curves are elliptic curves.
instance (GaloisField k, BCurve c k) => Curve B c k where

  data instance Point B c k = A k k -- ^ Affine point.
                            | O     -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Show)

  id = O
  {-# INLINE id #-}

  inv O       = O
  inv (A x y) = A x (x + y)
  {-# INLINE inv #-}

  add p O          = p
  add O q          = q
  add p@(A x1 y1) (A x2 y2)
    | xx /= 0      = A x3 y3
    | yy + x2 /= 0 = double p
    | otherwise    = O
    where
      xx = x1 + x2
      yy = y1 + y2
      l  = yy / xx
      x3 = l * (l + 1) + xx + a_ (witness :: c)
      y3 = l * (x1 + x3) + x3 + y1
  {-# INLINE add #-}

  double O       = O
  double (A x y) = A x' y'
    where
      l  = x + y / x
      l' = l + 1
      x' = l * l' + a_ (witness :: c)
      y' = x * x + l' * x'
  {-# INLINE double #-}

  def O       = True
  def (A x y) = ((x + a) * x + y) * x + b + y * y == 0
    where
      a = a_ (witness :: c)
      b = b_ (witness :: c)
  {-# INLINE def #-}

  disc _ = b_ (witness :: c)
  {-# INLINE disc #-}

  point 0 = A 0 <$> sr (b_ (witness :: c))
  point x = A x <$> quad 1 x ((x + a) * x * x + b)
    where
      a = a_ (witness :: c)
      b = b_ (witness :: c)
  {-# INLINE point #-}
