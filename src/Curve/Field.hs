module Curve.Field
  ( FGroup
  ) where

import Protolude

import Control.Monad.Random (Random(..))
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..), suchThatMap)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Group(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Field groups.
newtype FGroup k = F k
  deriving (Eq, Generic, NFData, Read, Show)

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- Field groups are groups.
instance GaloisField k => Group (FGroup k) where

  def (F x) = x /= 0
  {-# INLINE def #-}

  inv (F x) = F (recip x)
  {-# INLINE inv #-}

  mul (F x) n = F (pow x n)
  {-# INLINE mul #-}

-- Field groups are monoids.
instance GaloisField k => Monoid (FGroup k) where

  mempty = F 1
  {-# INLINE mempty #-}

-- Field groups are semigroups.
instance GaloisField k => Semigroup (FGroup k) where

  F x <> F y = F (x * y)
  {-# INLINE (<>) #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Field groups are arbitrary.
instance GaloisField k => Arbitrary (FGroup k) where
  arbitrary = suchThatMap arbitrary point
    where
      point 0 = Nothing
      point x = Just (F x)

-- Field groups are pretty.
instance GaloisField k => Pretty (FGroup k) where
  pretty (F x) = pretty x

-- Field groups are random.
instance GaloisField k => Random (FGroup k) where
  random g = case random g of
    (0, g') -> random g'
    (x, g') -> (F x, g')
  {-# INLINE random #-}
  randomR  = panic "not implemented."
