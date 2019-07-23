module Curve.Edwards
  ( ECurve(..)
  , EPoint
  , Point(..)
  ) where

import Protolude

import Control.Monad.Random (Random(..))
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..), suchThatMap)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Curve(..), Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Edwards curve representation.
data E

-- | Edwards curve points.
type EPoint = Point E

-- | Edwards curves @Ax^2 + y^2 = 1 + Dx^2y^2@.
class Curve E c k => ECurve c k where
  {-# MINIMAL a_, d_, g_, h_, n_, p_ #-}
  a_ :: c -> k                -- ^ Coefficient @A@.
  d_ :: c -> k                -- ^ Coefficient @D@.
  g_ :: EPoint c k            -- ^ Curve generator.
  h_ :: EPoint c k -> Integer -- ^ Curve cofactor.
  n_ :: EPoint c k -> Integer -- ^ Curve order.
  p_ :: EPoint c k -> Integer -- ^ Curve characteristic.

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- Edwards curves are elliptic curves.
instance (GaloisField k, ECurve c k) => Curve E c k where

  data instance Point E c k = A k k -- ^ Affine point.
    deriving (Eq, Generic, NFData, Read, Show)

  cof = h_
  {-# INLINE cof #-}

  disc _ = d * (1 - d)
    where
      d = d_ (witness :: c)
  {-# INLINE disc #-}

  gen = g_
  {-# INLINE gen #-}

  order = n_
  {-# INLINE order #-}

  point x = A x <$> yX (witness :: EPoint c k) x
  {-# INLINE point #-}

  yX _ x = sr ((1 - a * xx) / (1 - d * xx))
    where
      a  = a_ (witness :: c)
      d  = d_ (witness :: c)
      xx = x * x
  {-# INLINE yX #-}

-- Edwards points are groups.
instance (GaloisField k, ECurve c k) => Group (EPoint c k) where

  def (A x y) = a * xx + yy == 1 + d * xx * yy
    where
      a  = a_ (witness :: c)
      d  = d_ (witness :: c)
      xx = x * x
      yy = y * y
  {-# INLINE def #-}

  inv (A x y) = A (-x) y
  {-# INLINE inv #-}

-- Edwards points are monoids.
instance (GaloisField k, ECurve c k) => Monoid (EPoint c k) where

  mempty = A 0 1
  {-# INLINE mempty #-}
 
-- Edwards points are semigroups.
instance (GaloisField k, ECurve c k) => Semigroup (EPoint c k) where

  A x1 y1 <> A x2 y2 = A x3 y3
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
  {-# INLINE (<>) #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Edwards points are arbitrary.
instance (GaloisField k, ECurve c k) => Arbitrary (Point E c k) where
  arbitrary = suchThatMap arbitrary point

-- Edwards points are pretty.
instance (GaloisField k, ECurve c k) => Pretty (Point E c k) where
  pretty (A x y) = pretty (x, y)

-- Edwards points are random.
instance (GaloisField k, ECurve c k) => Random (Point E c k) where
  random g = case point x of
    Just p -> (p, g')
    _      -> random g'
    where
      (x, g') = random g
  {-# INLINE random #-}
  randomR  = panic "not implemented."
