module Group
  ( Group(..)
  ) where

import Protolude

import Control.Monad.Random (MonadRandom, Random, getRandom)
import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Groups.
class (Arbitrary g, Eq g, Generic g, Monoid g,
       Pretty g, Random g, Read g, Show g) => Group g where
  {-# MINIMAL def, gen, inv, order #-}

  -- | Well defined.
  def :: g -> Bool

  -- | Element doubling.
  double :: g -> g
  double = join (<>)
  {-# INLINE double #-}

  -- | Group generator.
  gen :: g

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

  -- | Curve order.
  order :: g -> Integer

  -- | Random element.
  rnd :: MonadRandom m => m g
  rnd = getRandom
  {-# INLINE rnd #-}
