module Group.Field
  ( module Group
  , module Group.Field
  ) where

import Protolude

import Control.Monad.Random (Random(..))
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (PrimeField')
import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Field groups.
class (GaloisField q, GaloisField r, PrimeField' r) => FGroup q r where
  {-# MINIMAL g_, h_, q_, r_ #-}
  g_ :: Element q r            -- ^ Group generator.
  h_ :: Element q r -> Integer -- ^ Group cofactor.
  q_ :: Element q r -> Integer -- ^ Group characteristic.
  r_ :: Element q r -> Integer -- ^ Group order.

-- | Field elements.
newtype Element q r = F q
  deriving (Eq, Functor, Generic, NFData, Read, Show)

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- Field elements are groups.
instance FGroup q r => Group (Element q r) where

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
instance FGroup q r => Monoid (Element q r) where

  mempty = F 1
  {-# INLINABLE mempty #-}

-- Field elements are semigroups.
instance FGroup q r => Semigroup (Element q r) where

  F x <> F y = F (x * y)
  {-# INLINABLE (<>) #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Field elements are arbitrary.
instance FGroup q r => Arbitrary (Element q r) where

  -- Arbitrary group element.
  arbitrary = mul' gen <$> arbitrary
  {- Arbitrary field element.
  arbitrary = suchThatMap arbitrary defX
    where
      defX 0 = Nothing
      defX x = Just (F x)
  -}
  {-# INLINABLE arbitrary #-}

-- Field elements are pretty.
instance FGroup q r => Pretty (Element q r) where

  pretty (F x) = pretty x

-- Field elements are random.
instance FGroup q r => Random (Element q r) where

  -- Random group element.
  random = first (mul' gen) . random
  {- Random field element.
  random g = case random g of
    (0, g') -> random g'
    (x, g') -> (F x, g')
  -}
  {-# INLINABLE random #-}

  randomR  = panic "not implemented."
