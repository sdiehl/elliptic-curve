module Curve.Edwards
  ( Coordinates(..)
  , Curve(..)
  , ECurve(..)
  , EPoint
  , EACurve(..)
  , EAPoint
  , Group(..)
  , Point(..)
  ) where

import Protolude

import Control.Monad.Random (Random(..))
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..), suchThatMap)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Curve(..), Form(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Edwards form
-------------------------------------------------------------------------------

-- | Edwards points.
type EPoint = Point 'Edwards

-- | Edwards curves.
class Curve 'Edwards c e k => ECurve c e k where
  {-# MINIMAL a_, d_, h_, q_, r_ #-}
  a_ :: EPoint c e k -> k       -- ^ Coefficient @A@.
  d_ :: EPoint c e k -> k       -- ^ Coefficient @D@.
  h_ :: EPoint c e k -> Integer -- ^ Curve cofactor.
  q_ :: EPoint c e k -> Integer -- ^ Curve characteristic.
  r_ :: EPoint c e k -> Integer -- ^ Curve order.

-- | Edwards coordinates.
data Coordinates = Affine

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Edwards affine curves.
type EAPoint = EPoint 'Affine

-- | Edwards affine curves @Ax^2 + y^2 = 1 + Dx^2y^2@.
class ECurve 'Affine e k => EACurve e k where
  {-# MINIMAL g_, x_, y_ #-}
  g_ :: EAPoint e k -- ^ Curve generator.
  x_ :: e -> k      -- ^ Coordinate @X@.
  y_ :: e -> k      -- ^ Coordinate @Y@.

-- Edwards affine curves are elliptic curves.
instance (GaloisField k, EACurve e k) => Curve 'Edwards 'Affine e k where
  data instance Point 'Edwards 'Affine e k = A k k -- ^ Affine point.
    deriving (Eq, Generic, NFData, Read, Show)
  char      = q_
  {-# INLINE char #-}
  cof       = h_
  {-# INLINE cof #-}
  disc _    = d * (1 - d)
    where
      d = d_ (witness :: EAPoint e k)
  {-# INLINE disc #-}
  point x y = let p = A x y in if def p then Just p else Nothing
  {-# INLINE point #-}
  pointX x  = A x <$> yX (witness :: EAPoint e k) x
  {-# INLINE pointX #-}
  yX _ x    = sr ((1 - a * xx) / (1 - d * xx))
    where
      a  = a_ (witness :: EAPoint e k)
      d  = d_ (witness :: EAPoint e k)
      xx = x * x
  {-# INLINE yX #-}

-- Edwards affine points are arbitrary.
instance (GaloisField k, EACurve e k) => Arbitrary (EAPoint e k) where
  arbitrary = suchThatMap arbitrary pointX

-- Edwards affine points are groups.
instance (GaloisField k, EACurve e k) => Group (EAPoint e k) where
  def (A x y) = a * xx + yy == 1 + d * xx * yy
    where
      a  = a_ (witness :: EAPoint e k)
      d  = d_ (witness :: EAPoint e k)
      xx = x * x
      yy = y * y
  {-# INLINE def #-}
  gen         = g_
  {-# INLINE gen #-}
  inv (A x y) = A (-x) y
  {-# INLINE inv #-}
  order       = r_
  {-# INLINE order #-}

-- Edwards affine points are monoids.
instance (GaloisField k, EACurve e k) => Monoid (EAPoint e k) where
  mempty = A 0 1
  {-# INLINE mempty #-}

-- Edwards affine points are pretty.
instance (GaloisField k, EACurve e k) => Pretty (EAPoint e k) where
  pretty (A x y) = pretty (x, y)

-- Edwards affine points are random.
instance (GaloisField k, EACurve e k) => Random (EAPoint e k) where
  random g = case pointX x of
    Just p -> (p, g')
    _      -> random g'
    where
      (x, g') = random g
  {-# INLINE random #-}
  randomR  = panic "not implemented."

-- Edwards affine points are semigroups.
instance (GaloisField k, EACurve e k) => Semigroup (EAPoint e k) where
  A x1 y1 <> A x2 y2 = A x3 y3
    where
      a    = a_ (witness :: EAPoint e k)
      d    = d_ (witness :: EAPoint e k)
      x1x2 = x1 * x2
      y1y2 = y1 * y2
      x1y2 = x1 * y2
      x2y1 = x2 * y1
      dxy  = d * x1x2 * y1y2
      x3   = (x1y2 + x2y1) / (1 + dxy)
      y3   = (y1y2 - a * x1x2) / (1 - dxy)
  {-# INLINE (<>) #-}
