module Group.Field
  ( module Group
  , module Group.Field
  ) where

import Protolude

import Control.Monad.Random (Random(..))
import Data.Field.Galois (GaloisField(..), PrimeField)
import Test.Tasty.QuickCheck (Arbitrary(..))
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Group (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Field groups.
class (GaloisField q, PrimeField r) => Field r q where
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
instance Field r q => Group (Element r q) where

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
instance Field r q => Monoid (Element r q) where

  mempty = F 1
  {-# INLINABLE mempty #-}

-- Field elements are semigroups.
instance Field r q => Semigroup (Element r q) where

  F x <> F y = F (x * y)
  {-# INLINABLE (<>) #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Field elements are arbitrary.
instance Field r q => Arbitrary (Element r q) where

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
instance Field r q => Pretty (Element r q) where

  pretty (F x) = pretty x

-- Field elements are random.
instance Field r q => Random (Element r q) where

  -- Random group element.
  random = first (mul' gen) . random
  {- Random field element.
  random g = case random g of
    (0, g') -> random g'
    (x, g') -> (F x, g')
  -}
  {-# INLINABLE random #-}

  randomR  = panic "not implemented."
