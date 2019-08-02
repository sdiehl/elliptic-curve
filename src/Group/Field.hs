module Group.Field
  ( Element
  , FGroup(..)
  , element
  , pattern F
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
newtype Element k = F' k
  deriving (Eq, Generic, NFData, Read, Show)

-- | Field elements patterns.
pattern F :: FGroup k => k -> Element k
pattern F x <- F' x

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- Field elements are groups.
instance FGroup k => Group (Element k) where

  add = (<>)
  {-# INLINE add #-}

  dbl = join (<>)
  {-# INLINE dbl #-}

  def (F' x) = x /= 0
  {-# INLINE def #-}

  gen = g_
  {-# INLINE gen #-}

  id = mempty
  {-# INLINE id #-}

  inv (F' x) = F' (recip x)
  {-# INLINE inv #-}

  mul' (F' x) n = F' (pow x n)
  {-# INLINE mul' #-}

  order = r_
  {-# INLINE order #-}

-- Field elements are monoids.
instance FGroup k => Monoid (Element k) where

  mempty = F' 1
  {-# INLINE mempty #-}

-- Field elements are semigroups.
instance FGroup k => Semigroup (Element k) where

  F' x <> F' y = F' (x * y)
  {-# INLINE (<>) #-}

-- Field element constructor.
element :: FGroup k => k -> Maybe (Element k)
element x = let p = F' x in if def p then Just p else Nothing

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- Field elements are arbitrary.
instance FGroup k => Arbitrary (Element k) where
  arbitrary = suchThatMap arbitrary defX
    where
      defX 0 = Nothing
      defX x = Just (F' x)

-- Field elements are pretty.
instance FGroup k => Pretty (Element k) where
  pretty (F' x) = pretty x

-- Field elements are random.
instance FGroup k => Random (Element k) where
  random g = case random g of
    (0, g') -> random g'
    (x, g') -> (F' x, g')
  {-# INLINE random #-}
  randomR  = panic "not implemented."
