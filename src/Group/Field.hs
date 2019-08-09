module Group.Field
  ( module Group
  , module Group.Field
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
class GaloisField k => FGroup k where
  {-# MINIMAL g_, q_, r_, x_ #-}
  g_ :: Element k            -- ^ Group generator.
  q_ :: Element k -> Integer -- ^ Group characteristic.
  r_ :: Element k -> Integer -- ^ Group order.
  x_ :: k                    -- ^ Group element.

-- | Field elements.
newtype Element k = F k
  deriving (Eq, Functor, Generic, NFData, Read, Show)

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- Field elements are groups.
instance FGroup k => Group (Element k) where

  add = (<>)
  {-# INLINABLE add #-}

  dbl = join (<>)
  {-# INLINABLE dbl #-}

  def (F x) = x /= 0
  {-# INLINABLE def #-}

  gen = g_
  {-# INLINABLE gen #-}

  id = mempty
  {-# INLINABLE id #-}

  inv (F x) = F (recip x)
  {-# INLINABLE inv #-}

  mul' (F x) n = F (pow x n)
  {-# INLINABLE mul' #-}

  order = r_
  {-# INLINABLE order #-}

-- Field elements are monoids.
instance FGroup k => Monoid (Element k) where

  mempty = F 1
  {-# INLINABLE mempty #-}

-- Field elements are semigroups.
instance FGroup k => Semigroup (Element k) where

  F x <> F y = F (x * y)
  {-# INLINABLE (<>) #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Field elements are arbitrary.
instance FGroup k => Arbitrary (Element k) where

  arbitrary = suchThatMap arbitrary defX
    where
      defX 0 = Nothing
      defX x = Just (F x)
  {-# INLINABLE arbitrary #-}

-- Field elements are pretty.
instance FGroup k => Pretty (Element k) where

  pretty (F x) = pretty x

-- Field elements are random.
instance FGroup k => Random (Element k) where

  random g = case random g of
    (0, g') -> random g'
    (x, g') -> (F x, g')
  {-# INLINABLE random #-}

  randomR  = panic "not implemented."
