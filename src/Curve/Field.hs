module Curve.Field
  ( FCurve(..)
  , FPoint
  , Point(..)
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

-- | Field curve representation.
data F

-- | Field curve points.
type FPoint = Point F

-- | Field curves @y^2 = x^3 + Ax + B@.
class Curve F c k => FCurve c k where
  {-# MINIMAL g_, h_, n_, p_ #-}
  g_ :: FPoint c k            -- ^ Curve generator.
  h_ :: FPoint c k -> Integer -- ^ Curve cofactor.
  n_ :: FPoint c k -> Integer -- ^ Curve order.
  p_ :: FPoint c k -> Integer -- ^ Curve characteristic.

-- Field points are arbitrary.
instance (GaloisField k, FCurve c k) => Arbitrary (Point F c k) where
  arbitrary = suchThatMap arbitrary point

-- Field points are pretty.
instance (GaloisField k, FCurve c k) => Pretty (Point F c k) where
  pretty (K k) = pretty k

-- Field points are random.
instance (GaloisField k, FCurve c k) => Random (Point F c k) where
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

-- Field curves are elliptic curves.
instance (GaloisField k, FCurve c k) => Curve F c k where

  data instance Point F c k = K k -- ^ Field element.
    deriving (Eq, Generic, NFData, Read, Show)

  id = K 1
  {-# INLINE id #-}

  inv (K x) = K (recip x)
  {-# INLINE inv #-}

  add (K x) (K y) = K (x * y)
  {-# INLINE add #-}

  cof = h_
  {-# INLINE cof #-}

  def = (/= K 0)
  {-# INLINE def #-}

  disc = const 1
  {-# INLINE disc #-}

  gen = g_
  {-# INLINE gen #-}

  order = n_
  {-# INLINE order #-}

  point 0 = Nothing
  point x = Just (K x)
  {-# INLINE point #-}

  rnd = getRandom
  {-# INLINE rnd #-}

  yX = panic "not implemented." -- TODO
  {-# INLINE yX #-}
