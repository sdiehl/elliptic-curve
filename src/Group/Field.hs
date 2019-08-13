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
class (GaloisField q, GaloisField r, PrimeField' r) => FGroup r q where
  {-# MINIMAL g_, h_, q_, r_ #-}
  g_ :: Element r q            -- ^ Group generator.
  h_ :: Element r q -> Integer -- ^ Group cofactor.
  q_ :: Element r q -> Integer -- ^ Group characteristic.
  r_ :: Element r q -> Integer -- ^ Group order.

-- | Field elements.
newtype Element r q = F q
  deriving (Eq, Functor, Generic, NFData, Read, Show)

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- Field elements are groups.
instance FGroup r q => Group (Element r q) where

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

  inv = (<$>) recip
  {-# INLINABLE inv #-}

  mul' = (. flip pow) . flip (<$>)
  {-# INLINABLE mul' #-}

  order = r_
  {-# INLINABLE order #-}

-- Field elements are monoids.
instance FGroup r q => Monoid (Element r q) where

  mempty = F 1
  {-# INLINABLE mempty #-}

-- Field elements are semigroups.
instance FGroup r q => Semigroup (Element r q) where

  F x <> F y = F (x * y)
  {-# INLINABLE (<>) #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Field elements are arbitrary.
instance FGroup r q => Arbitrary (Element r q) where

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
instance FGroup r q => Pretty (Element r q) where

  pretty (F x) = pretty x

-- Field elements are random.
instance FGroup r q => Random (Element r q) where

  -- Random group element.
  random = first (mul' gen) . random
  {- Random field element.
  random g = case random g of
    (0, g') -> random g'
    (x, g') -> (F x, g')
  -}
  {-# INLINABLE random #-}

  randomR  = panic "not implemented."
