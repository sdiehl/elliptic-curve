module Curve
  ( Curve(..)
  , Group(..)
  ) where

import Protolude

import Control.Monad.Random (MonadRandom, Random, getRandom)
import GaloisField (GaloisField)
import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Elliptic curves.
class (GaloisField k, Group (Point r c k)) => Curve r c k where
  {-# MINIMAL cof, disc, gen, order, point, yX #-}

  -- | Curve point.
  data family Point r c k :: *

  -- | Curve cofactor.
  cof :: Point r c k -> Integer

  -- | Curve discriminant.
  disc :: Point r c k -> k

  -- | Curve generator.
  gen :: Point r c k

  -- | Curve order.
  order :: Point r c k -> Integer

  -- | Point from X coordinate.
  point :: k -> Maybe (Point r c k)

  -- | Y coordinate from X coordinate.
  yX :: Point r c k -> k -> Maybe k

-- | Groups.
class (Arbitrary g, Eq g, Generic g, Monoid g,
       Pretty g, Random g, Read g, Show g) => Group g where
  {-# MINIMAL def, inv #-}

  -- | Well defined.
  def :: g -> Bool

  -- | Element doubling.
  double :: g -> g
  double = join (<>)
  {-# INLINE double #-}

  -- | Element inversion.
  inv :: g -> g

  -- | Element multiplication.
  mul :: g -> Integer -> g
  mul p n
    | n < 0     = inv (mul p (-n))
    | n == 0    = mempty
    | n == 1    = p
    | even n    = p'
    | otherwise = p <> p'
    where
      p' = mul (p <> p) (div n 2)
  {-# INLINE mul #-}

  -- | Random element.
  rnd :: MonadRandom m => m g
  rnd = getRandom
  {-# INLINE rnd #-}
