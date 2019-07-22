module Curve.Binary
  ( BCurve(..)
  , BPoint
  , Point(..)
  ) where

import Protolude

import Control.Monad.Random (Random(..), getRandom)
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Leijen.Text (Pretty(..))

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

-- Binary points are arbitrary.
instance (GaloisField k, BCurve c k) => Arbitrary (Point B c k) where
  arbitrary = return g_ -- TODO
  -- arbitrary = mul g_ <$> (arbitrary :: Gen Int)
  -- arbitrary = suchThatMap arbitrary point

-- Binary points are pretty.
instance (GaloisField k, BCurve c k) => Pretty (Point B c k) where
  pretty (A x y) = pretty (x, y)
  pretty O       = "O"

-- Binary points are random.
instance (GaloisField k, BCurve c k) => Random (Point B c k) where
  random g = case point x of
    Just p -> (p, g')
    _      -> random g'
    where
      (x, g') = random g
  {-# INLINE random #-}
  randomR  = panic "not implemented."

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- Binary curves are elliptic curves.
instance (GaloisField k, BCurve c k) => Curve B c k where

  data instance Point B c k = A k k -- ^ Affine point.
                            | O     -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)

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

  rnd = getRandom
  {-# INLINE rnd #-}
