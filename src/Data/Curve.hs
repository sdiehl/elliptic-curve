module Data.Curve
  (
  -- * Elliptic curves
    Curve(..)
  -- ** Elliptic curve forms
  , Form(..)
  -- ** Elliptic curve coordinates
  , Coordinates(..)
  ) where

import Protolude

import Control.Monad.Random (MonadRandom, Random(..), getRandom)
import Data.Field.Galois (GaloisField, PrimeField(..), TowerOfFields(..))
import Data.Group as G (Group(..))
import GHC.Natural (Natural)
import Test.Tasty.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Elliptic curves.
class (GaloisField q, PrimeField r, Arbitrary (Point f c e q r),
       Eq (Point f c e q r), Generic (Point f c e q r), Group (Point f c e q r),
       NFData (Point f c e q r), Pretty (Point f c e q r), Random (Point f c e q r),
       Show (Point f c e q r)) => Curve (f :: Form) (c :: Coordinates) e q r where
  {-# MINIMAL add, char, cof, dbl, def, disc, fromA, gen,
              id, inv, line, order, point, pointX, toA, yX #-}

  -- | Curve point.
  data family Point f c e q r :: *

  -- Parameters

  -- | Curve characteristic.
  char :: Point f c e q r -> Natural

  -- | Curve cofactor.
  cof :: Point f c e q r -> Natural

  -- | Curve well-defined.
  def :: Point f c e q r -> Bool

  -- | Curve discriminant.
  disc :: Point f c e q r -> q

  -- | Curve order.
  order :: Point f c e q r -> Natural

  -- Operations

  -- | Point addition.
  add :: Point f c e q r -> Point f c e q r -> Point f c e q r

  -- | Point doubling.
  dbl :: Point f c e q r -> Point f c e q r

  -- | Point identity.
  id :: Point f c e q r

  -- | Point inversion.
  inv :: Point f c e q r -> Point f c e q r

  -- | Point multiplication by field element.
  mul :: Point f c e q r -> r -> Point f c e q r
  mul = (. fromP) . mul'
  {-# INLINABLE mul #-}

  -- | Point multiplication by integral element.
  mul' :: Integral n => Point f c e q r -> n -> Point f c e q r
  mul' p n
    | n < 0     = inv (mul' p (-n))
    | n == 0    = id
    | n == 1    = p
    | even n    = p'
    | otherwise = add p p'
    where
      p' = mul' (dbl p) (div n 2)
  {-# INLINABLE mul' #-}

  -- Points

  -- | Curve generator.
  gen :: Point f c e q r

  -- | Line function on subfield.
  line :: (TowerOfFields q' q, PrimeField r', Curve f c e q' r')
    => Point f c e q r -> Point f c e q r -> Point f c e q' r' -> q

  -- | Get point from X and Y coordinates.
  point :: q -> q -> Maybe (Point f c e q r)

  -- | Get point from X coordinate.
  pointX :: q -> Maybe (Point f c e q r)

  -- | Random point.
  rnd :: MonadRandom m => m (Point f c e q r)
  rnd = getRandom

  -- | Get Y coordinate from X coordinate.
  yX :: Point f c e q r -> q -> Maybe q

  -- Transformations

  -- | Transform from affine coordinates.
  fromA :: Curve f 'Affine e q r => Point f 'Affine e q r -> Point f c e q r

  -- | Transform to affine coordinates.
  toA :: Curve f 'Affine e q r => Point f c e q r -> Point f 'Affine e q r

{-# SPECIALISE mul' ::
  Point f c e q r => g -> Int -> g
  Point f c e q r => g -> Integer -> g
  Point f c e q r => g -> Natural -> g
  Point f c e q r => g -> Word -> g
  #-}

-- | Curve forms.
data Form = Binary
          | Edwards
          | Montgomery
          | Weierstrass

-- | Curve coordinates.
data Coordinates = Affine
                 | Jacobian
                 | Projective

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Elliptic curve points are arbitrary.
instance Curve f c e q r => Arbitrary (Point f c e q r) where

  -- Arbitrary group element.
  arbitrary = mul gen <$> arbitrary
  {- Arbitrary curve point.
  arbitrary = suchThatMap arbitrary pointX
  -}
  {-# INLINABLE arbitrary #-}

-- Elliptic curve points are monoids.
instance Curve f c e q r => Monoid (Point f c e q r) where

  mempty = id
  {-# INLINABLE mempty #-}

-- Elliptic curve points are random.
instance Curve f c e q r => Random (Point f c e q r) where

  -- Random group element.
  random  = first (mul gen) . random
  {- Random curve point.
  random g = case pointX x of
    Just p -> (p, g')
    _      -> random g'
    where
      (x, g') = random g
  -}
  {-# INLINABLE random #-}

  randomR = panic "Curve.randomR: not implemented."

-- Elliptic curve points are groups.
instance Curve f c e q r => G.Group (Point f c e q r) where

  invert = inv
  {-# INLINABLE invert #-}

  pow = mul'
  {-# INLINABLE pow #-}

-- Elliptic curve points are semigroups.
instance Curve f c e q r => Semigroup (Point f c e q r) where

  p <> q = if p == q then dbl p else add p q
  {-# INLINABLE (<>) #-}
