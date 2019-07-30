module Curve.Montgomery
  ( Coordinates(..)
  , Curve(..)
  , Group(..)
  , MCurve(..)
  , MPoint
  , MACurve(..)
  , MAPoint
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
-- Montgomery form
-------------------------------------------------------------------------------

-- | Montgomery points.
type MPoint = Point 'Montgomery

-- | Montgomery curves.
class Curve 'Montgomery c e k => MCurve c e k where
  {-# MINIMAL a_, b_, h_, q_, r_ #-}
  a_ :: MPoint c e k -> k       -- ^ Coefficient @A@.
  b_ :: MPoint c e k -> k       -- ^ Coefficient @B@.
  h_ :: MPoint c e k -> Integer -- ^ Curve cofactor.
  q_ :: MPoint c e k -> Integer -- ^ Curve characteristic.
  r_ :: MPoint c e k -> Integer -- ^ Curve order.

-- | Montgomery coordinates.
data Coordinates = Affine

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Montgomery affine points.
type MAPoint = MPoint 'Affine

-- | Montgomery affine curves @By^2 = x^3 + Ax^2 + x@.
class MCurve 'Affine e k => MACurve e k where
  {-# MINIMAL gA_, xA_, yA_ #-}
  gA_ :: MAPoint e k -- ^ Curve generator.
  xA_ :: e -> k      -- ^ Coordinate @X@.
  yA_ :: e -> k      -- ^ Coordinate @Y@.

-- Montgomery affine curves are elliptic curves.
instance (GaloisField k, MACurve e k) => Curve 'Montgomery 'Affine e k where
  data instance Point 'Montgomery 'Affine e k = A k k -- ^ Affine point.
                                              | O     -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)
  char      = q_
  {-# INLINE char #-}
  cof       = h_
  {-# INLINE cof #-}
  disc _    = b * (a * a - 4)
    where
      a = a_ (witness :: MAPoint e k)
      b = b_ (witness :: MAPoint e k)
  {-# INLINE disc #-}
  point x y = let p = A x y in if def p then Just p else Nothing
  {-# INLINE point #-}
  pointX x  = A x <$> yX (witness :: MAPoint e k) x
  {-# INLINE pointX #-}
  yX _ x    = sr ((((x + a) * x) + 1) * x / b)
    where
      a  = a_ (witness :: MAPoint e k)
      b  = b_ (witness :: MAPoint e k)
  {-# INLINE yX #-}

-- Montgomery affine points are arbitrary.
instance (GaloisField k, MACurve e k) => Arbitrary (MAPoint e k) where
  arbitrary = suchThatMap arbitrary pointX

-- Montgomery affine points are groups.
instance (GaloisField k, MACurve e k) => Group (MAPoint e k) where
  def O          = True
  def (A x y)    = b * y * y == (((x + a) * x) + 1) * x
    where
      a = a_ (witness :: MAPoint e k)
      b = b_ (witness :: MAPoint e k)
  {-# INLINE def #-}
  double O       = O
  double (A _ 0) = O
  double (A x y) = A x' y'
    where
      a  = a_ (witness :: MAPoint e k)
      b  = b_ (witness :: MAPoint e k)
      l  = (x * (3 * x + 2 * a) + 1) / (2 * b * y)
      x' = b * l * l - a - 2 * x
      y' = l * (x - x') - y
  {-# INLINE double #-}
  gen            = gA_
  {-# INLINE gen #-}
  inv O          = O
  inv (A x y)    = A x (-y)
  {-# INLINE inv #-}
  order          = r_
  {-# INLINE order #-}

-- Montgomery affine points are monoids.
instance (GaloisField k, MACurve e k) => Monoid (MAPoint e k) where
  mempty = O
  {-# INLINE mempty #-}

-- Montgomery affine points are pretty.
instance (GaloisField k, MACurve e k) => Pretty (MAPoint e k) where
  pretty (A x y) = pretty (x, y)
  pretty O       = "O"

-- Montgomery affine points are random.
instance (GaloisField k, MACurve e k) => Random (MAPoint e k) where
  random g = case pointX x of
    Just p -> (p, g')
    _      -> random g'
    where
      (x, g') = random g
  {-# INLINE random #-}
  randomR  = panic "not implemented."

-- Montgomery affine points are semigroups.
instance (GaloisField k, MACurve e k) => Semigroup (MAPoint e k) where
  p <> O           = p
  O <> q           = q
  p@(A x1 y1) <> A x2 y2
    | x1 /= x2     = A x3 y3
    | y1 + y2 == 0 = O
    | otherwise    = double p
    where
      a  = a_ (witness :: MAPoint e k)
      b  = b_ (witness :: MAPoint e k)
      l  = (y2 - y1) / (x2 - x1)
      x3 = b * l * l - a - x1 - x2
      y3 = l * (x1 - x3) - y1
  {-# INLINE (<>) #-}
