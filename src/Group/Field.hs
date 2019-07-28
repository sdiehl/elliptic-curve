module Group.Field
  ( FGroup(..)
  , Element(..)
  ) where

import Protolude

import Control.Monad.Random (Random(..))
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..), suchThatMap)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Field groups.
class FGroup k where
  {-# MINIMAL g_, q_, r_, x_ #-}
  g_ :: Element k            -- ^ Group generator.
  q_ :: Element k -> Integer -- ^ Group characteristic.
  r_ :: Element k -> Integer -- ^ Group order.
  x_ :: k                    -- ^ Group element.

-- | Field elements.
newtype Element k = F k
  deriving (Eq, Generic, NFData, Read, Show)

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- Field elements are groups.
instance (GaloisField k, FGroup k) => Group (Element k) where

  def (F x) = x /= 0
  {-# INLINE def #-}

  gen = g_
  {-# INLINE gen #-}

  inv (F x) = F (recip x)
  {-# INLINE inv #-}

  mul' (F x) n = F (pow x n)
  {-# INLINE mul' #-}

  order = r_
  {-# INLINE order #-}

-- Field elements are monoids.
instance (GaloisField k, FGroup k) => Monoid (Element k) where

  mempty = F 1
  {-# INLINE mempty #-}

-- Field elements are semigroups.
instance (GaloisField k, FGroup k) => Semigroup (Element k) where

  F x <> F y = F (x * y)
  {-# INLINE (<>) #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Field elements are arbitrary.
instance (GaloisField k, FGroup k) => Arbitrary (Element k) where
  arbitrary = suchThatMap arbitrary defX
    where
      defX 0 = Nothing
      defX x = Just (F x)

-- Field elements are pretty.
instance (GaloisField k, FGroup k) => Pretty (Element k) where
  pretty (F x) = pretty x

-- Field elements are random.
instance (GaloisField k, FGroup k) => Random (Element k) where
  random g = case random g of
    (0, g') -> random g'
    (x, g') -> (F x, g')
  {-# INLINE random #-}
  randomR  = panic "not implemented."
