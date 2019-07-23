module Curve.Montgomery
  ( Point(..)
  , MCurve(..)
  , MPoint
  ) where

import Protolude

import Control.Monad.Random (Random(..), getRandom)
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..), suchThatMap)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Curve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Montgomery curve representation.
data M

-- | Montgomery curve points.
type MPoint = Point M

-- | Montgomery curves @By^2 = x^3 + Ax^2 + x@.
class Curve M c k => MCurve c k where
  {-# MINIMAL a_, b_, g_, h_, n_, p_ #-}
  a_ :: c -> k                -- ^ Coefficient @A@.
  b_ :: c -> k                -- ^ Coefficient @B@.
  g_ :: MPoint c k            -- ^ Curve generator.
  h_ :: MPoint c k -> Integer -- ^ Curve cofactor.
  n_ :: MPoint c k -> Integer -- ^ Curve order.
  p_ :: MPoint c k -> Integer -- ^ Curve characteristic.

-- Montgomery points are arbitrary.
instance (GaloisField k, MCurve c k) => Arbitrary (Point M c k) where
  arbitrary = suchThatMap arbitrary point

-- Montgomery points are pretty.
instance (GaloisField k, MCurve c k) => Pretty (Point M c k) where
  pretty (A x y) = pretty (x, y)
  pretty O       = "O"

-- Montgomery points are random.
instance (GaloisField k, MCurve c k) => Random (Point M c k) where
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

-- Montgomery curves are elliptic curves.
instance (GaloisField k, MCurve c k) => Curve M c k where

  data instance Point M c k = A k k -- ^ Affine point.
                            | O     -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)

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
  double (A _ 0) = O
  double (A x y) = A x' y'
    where
      a  = a_ (witness :: c)
      b  = b_ (witness :: c)
      l  = (x * (3 * x + 2 * a) + 1) / (2 * b * y)
      x' = b * l * l - a - 2 * x
      y' = l * (x - x') - y
  {-# INLINE double #-}

  cof = h_
  {-# INLINE cof #-}

  def O       = True
  def (A x y) = b * y * y == (((x + a) * x) + 1) * x
    where
      a = a_ (witness :: c)
      b = b_ (witness :: c)
  {-# INLINE def #-}

  disc _ = b * (a * a - 4)
    where
      a = a_ (witness :: c)
      b = b_ (witness :: c)
  {-# INLINE disc #-}

  gen = g_
  {-# INLINE gen #-}

  order = n_
  {-# INLINE order #-}

  point x = A x <$> yX (witness :: MPoint c k) x
  {-# INLINE point #-}

  rnd = getRandom
  {-# INLINE rnd #-}

  yX _ x = sr ((((x + a) * x) + 1) * x / b)
    where
      a  = a_ (witness :: c)
      b  = b_ (witness :: c)
  {-# INLINE yX #-}
