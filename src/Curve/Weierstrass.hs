module Curve.Weierstrass
  ( Curve(..)
  , Group(..)
  , Point(..)
  , WCurve(..)
  , WPoint
  ) where

import Protolude

import Control.Monad.Random (Random(..), RandomGen)
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..), Gen)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Curve(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Weierstrass curve representation.
data W

-- | Weierstrass curve points.
type WPoint = Point W

-- | Weierstrass curves @y^2 = x^3 + Ax + B@.
class Curve W c k => WCurve c k where
  {-# MINIMAL a_, b_, g_, h_, q_, r_, x_, y_ #-}
  a_ :: c -> k                -- ^ Coefficient @A@.
  b_ :: c -> k                -- ^ Coefficient @B@.
  g_ :: WPoint c k            -- ^ Curve generator.
  h_ :: WPoint c k -> Integer -- ^ Curve cofactor.
  q_ :: WPoint c k -> Integer -- ^ Curve characteristic.
  r_ :: WPoint c k -> Integer -- ^ Curve order.
  x_ :: c -> k                -- ^ Coordinate @X@.
  y_ :: c -> k                -- ^ Coordinate @Y@.

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- Weierstrass curves are elliptic curves.
instance (GaloisField k, WCurve c k) => Curve W c k where

  data instance Point W c k = A k k -- ^ Affine point.
                            | O     -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)

  char = q_
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = 4 * a * a * a + 27 * b * b
    where
      a = a_ (witness :: c)
      b = b_ (witness :: c)
  {-# INLINE disc #-}

  point x y = let p = A x y in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = A x <$> yX (witness :: WPoint c k) x
  {-# INLINE pointX #-}

  yX _ x = sr (((x * x + a) * x) + b)
    where
      a = a_ (witness :: c)
      b = b_ (witness :: c)
  {-# INLINE yX #-}

-- Weierstrass points are groups.
instance (GaloisField k, WCurve c k) => Group (WPoint c k) where

  def O       = True
  def (A x y) = y * y == (x * x + a) * x + b
    where
      a = a_ (witness :: c)
      b = b_ (witness :: c)
  {-# INLINE def #-}

  double O       = O
  double (A _ 0) = O
  double (A x y) = A x' y'
    where
      l  = (3 * x * x + a_ (witness :: c)) / (2 * y)
      x' = l * l - 2 * x
      y' = l * (x - x') - y
  {-# INLINE double #-}

  gen = g_
  {-# INLINE gen #-}

  inv O       = O
  inv (A x y) = A x (-y)
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Weierstrass points are monoids.
instance (GaloisField k, WCurve c k) => Monoid (WPoint c k) where

  mempty = O
  {-# INLINE mempty #-}

-- Weierstrass points are semigroups.
instance (GaloisField k, WCurve c k) => Semigroup (WPoint c k) where

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

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Weierstrass points are arbitrary.
instance (GaloisField k, WCurve c k) => Arbitrary (Point W c k) where
  arbitrary = mul' g_ <$> (arbitrary :: Gen Integer) -- TODO
  -- arbitrary = suchThatMap arbitrary pointX

-- Weierstrass points are pretty.
instance (GaloisField k, WCurve c k) => Pretty (Point W c k) where
  pretty (A x y) = pretty (x, y)
  pretty O       = "O"

-- Weierstrass points are random.
instance (GaloisField k, WCurve c k) => Random (Point W c k) where
  random  = first (mul' g_) . (random :: RandomGen g => g -> (Integer, g)) -- TODO
  -- random g = case pointX x of
  --   Just p -> (p, g')
  --   _      -> random g'
  --   where
  --     (x, g') = random g
  {-# INLINE random #-}
  randomR = panic "not implemented."
