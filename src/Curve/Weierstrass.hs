module Curve.Weierstrass
  ( Coordinates(..)
  , Curve(..)
  , Group(..)
  , Point(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  ) where

import Protolude

import Control.Monad.Random (Random(..), RandomGen)
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..), Gen)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Curve(..), Form(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Weierstrass form
-------------------------------------------------------------------------------

-- | Weierstrass points.
type WPoint = Point 'Weierstrass

-- | Weierstrass curves.
class Curve 'Weierstrass c e k => WCurve c e k where
  {-# MINIMAL a_, b_, h_, q_, r_ #-}
  a_ :: WPoint c e k -> k       -- ^ Coefficient @A@.
  b_ :: WPoint c e k -> k       -- ^ Coefficient @B@.
  h_ :: WPoint c e k -> Integer -- ^ Curve cofactor.
  q_ :: WPoint c e k -> Integer -- ^ Curve characteristic.
  r_ :: WPoint c e k -> Integer -- ^ Curve order.

-- | Weierstrass coordinates.
data Coordinates = Affine

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Weierstrass affine points.
type WAPoint = WPoint 'Affine

-- | Weierstrass affine curves @y^2 = x^3 + Ax + B@.
class WCurve 'Affine e k => WACurve e k where
  {-# MINIMAL gA_, xA_, yA_ #-}
  gA_ :: WAPoint e k -- ^ Curve generator.
  xA_ :: e -> k      -- ^ Coordinate @X@.
  yA_ :: e -> k      -- ^ Coordinate @Y@.

-- Weierstrass affine curves are elliptic curves.
instance (GaloisField k, WACurve e k) => Curve 'Weierstrass 'Affine e k where

  data instance Point 'Weierstrass 'Affine e k = A k k -- ^ Affine point.
                                               | O     -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)

  char = q_
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = 4 * a * a * a + 27 * b * b
    where
      a = a_ (witness :: WAPoint e k)
      b = b_ (witness :: WAPoint e k)
  {-# INLINE disc #-}

  point x y = let p = A x y in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = A x <$> yX (witness :: WAPoint e k) x
  {-# INLINE pointX #-}

  yX _ x = sr (((x * x + a) * x) + b)
    where
      a = a_ (witness :: WAPoint e k)
      b = b_ (witness :: WAPoint e k)
  {-# INLINE yX #-}

-- Weierstrass affine points are arbitrary.
instance (GaloisField k, WACurve e k) => Arbitrary (WAPoint e k) where
  arbitrary = mul' gA_ <$> (arbitrary :: Gen Integer) -- TODO
  -- arbitrary = suchThatMap arbitrary pointX

-- Weierstrass affine points are groups.
instance (GaloisField k, WACurve e k) => Group (WAPoint e k) where
  def O          = True
  def (A x y)    = y * y == (x * x + a) * x + b
    where
      a = a_ (witness :: WAPoint e k)
      b = b_ (witness :: WAPoint e k)
  {-# INLINE def #-}
  double O       = O
  double (A _ 0) = O
  double (A x y) = A x' y'
    where
      l  = (3 * x * x + a_ (witness :: WAPoint e k)) / (2 * y)
      x' = l * l - 2 * x
      y' = l * (x - x') - y
  {-# INLINE double #-}
  gen            = gA_
  {-# INLINE gen #-}
  inv O          = O
  inv (A x y)    = A x (-y)
  {-# INLINE inv #-}
  order          = r_
  {-# INLINE order #-}

-- Weierstrass affine points are monoids.
instance (GaloisField k, WACurve e k) => Monoid (WAPoint e k) where
  mempty = O
  {-# INLINE mempty #-}

-- Weierstrass affine points are pretty.
instance (GaloisField k, WACurve e k) => Pretty (WAPoint e k) where
  pretty (A x y) = pretty (x, y)
  pretty O       = "O"

-- Weierstrass affine points are random.
instance (GaloisField k, WACurve e k) => Random (WAPoint e k) where
  random  = first (mul' gA_) . (random :: RandomGen g => g -> (Integer, g)) -- TODO
  -- random g = case pointX x of
  --   Just p -> (p, g')
  --   _      -> random g'
  --   where
  --     (x, g') = random g
  {-# INLINE random #-}
  randomR = panic "not implemented."

-- Weierstrass affine points are semigroups.
instance (GaloisField k, WACurve e k) => Semigroup (WAPoint e k) where
  p <> O           = p
  O <> q           = q
  p@(A x1 y1) <> A x2 y2
    | x1 /= x2     = A x3 y3
    | y1 + y2 == 0 = O
    | otherwise    = double p
    where
      l  = (y1 - y2) / (x1 - x2)
      x3 = l * l - x1 - x2
      y3 = l * (x1 - x3) - y1
  {-# INLINE (<>) #-}
