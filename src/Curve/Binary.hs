module Curve.Binary
  ( BCurve(..)
  , BPoint
  , BACurve(..)
  , BAPoint
  , Coordinates(..)
  , Curve(..)
  , Group(..)
  , Point(..)
  ) where

import Protolude

import Control.Monad.Random (Random(..))
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Curve(..), Form(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Binary form
-------------------------------------------------------------------------------

-- | Binary points.
type BPoint = Point 'Binary

-- | Binary curves.
class Curve 'Binary c e k => BCurve c e k where
  {-# MINIMAL a_, b_, h_, p_, r_ #-}
  a_ :: BPoint c e k -> k       -- ^ Coefficient @A@.
  b_ :: BPoint c e k -> k       -- ^ Coefficient @B@.
  h_ :: BPoint c e k -> Integer -- ^ Curve cofactor.
  p_ :: BPoint c e k -> Integer -- ^ Curve polynomial.
  r_ :: BPoint c e k -> Integer -- ^ Curve order.

-- | Binary coordinates.
data Coordinates = Affine

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Binary affine points.
type BAPoint = BPoint 'Affine

-- | Binary affine curves @y^2 + xy = x^3 + Ax^2 + B@.
class BCurve 'Affine e k => BACurve e k where
  {-# MINIMAL gA_, xA_, yA_ #-}
  gA_ :: BAPoint e k -- ^ Curve generator.
  xA_ :: e -> k      -- ^ Coordinate @X@.
  yA_ :: e -> k      -- ^ Coordinate @Y@.

-- Binary affine curves are elliptic curves.
instance (GaloisField k, BACurve e k) => Curve 'Binary 'Affine e k where
  data instance Point 'Binary 'Affine e k = A k k -- ^ Affine point.
                                          | O     -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)
  char      = const 2
  {-# INLINE char #-}
  cof       = h_
  {-# INLINE cof #-}
  disc _    = b_ (witness :: BAPoint e k)
  {-# INLINE disc #-}
  point x y = let p = A x y in if def p then Just p else Nothing
  {-# INLINE point #-}
  pointX x  = A x <$> yX (witness :: BAPoint e k) x
  {-# INLINE pointX #-}
  yX _ 0    = sr (b_ (witness :: BAPoint e k))
  yX _ x    = quad 1 x ((x + a) * x * x + b)
    where
      a = a_ (witness :: BAPoint e k)
      b = b_ (witness :: BAPoint e k)
  {-# INLINE yX #-}

-- Binary affine points are arbitrary.
instance (GaloisField k, BACurve e k) => Arbitrary (BAPoint e k) where
  arbitrary = return gA_ -- TODO
  -- arbitrary = mul gA_ <$> (arbitrary :: Gen Int)
  -- arbitrary = suchThatMap arbitrary pointX

-- Binary affine points are groups.
instance (GaloisField k, BACurve e k) => Group (BAPoint e k) where
  def O          = True
  def (A x y)    = ((x + a) * x + y) * x + b + y * y == 0
    where
      a = a_ (witness :: BAPoint e k)
      b = b_ (witness :: BAPoint e k)
  {-# INLINE def #-}
  double O       = O
  double (A x y) = A x' y'
    where
      l  = x + y / x
      l' = l + 1
      x' = l * l' + a_ (witness :: BAPoint e k)
      y' = x * x + l' * x'
  {-# INLINE double #-}
  gen            = gA_
  {-# INLINE gen #-}
  inv O          = O
  inv (A x y)    = A x (x + y)
  {-# INLINE inv #-}
  order          = r_
  {-# INLINE order #-}

-- Binary affine points are monoids.
instance (GaloisField k, BACurve e k) => Monoid (BAPoint e k) where
  mempty = O
  {-# INLINE mempty #-}

-- Binary affine points are pretty.
instance (GaloisField k, BACurve e k) => Pretty (BAPoint e k) where
  pretty (A x y) = pretty (x, y)
  pretty O       = "O"

-- Binary affine points are random.
instance (GaloisField k, BACurve e k) => Random (BAPoint e k) where
  random g = case pointX x of
    Just p -> (p, g')
    _      -> random g'
    where
      (x, g') = random g
  {-# INLINE random #-}
  randomR  = panic "not implemented."

-- Binary affine points are semigroups.
instance (GaloisField k, BACurve e k) => Semigroup (BAPoint e k) where
  p <> O           = p
  O <> q           = q
  p@(A x1 y1) <> A x2 y2
    | xx /= 0      = A x3 y3
    | yy + x2 /= 0 = double p
    | otherwise    = O
    where
      xx = x1 + x2
      yy = y1 + y2
      l  = yy / xx
      x3 = l * (l + 1) + xx + a_ (witness :: BAPoint e k)
      y3 = l * (x1 + x3) + x3 + y1
  {-# INLINE (<>) #-}
