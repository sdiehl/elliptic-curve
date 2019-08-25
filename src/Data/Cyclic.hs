module Data.Cyclic
  ( Cyclic(..)
  ) where

import Protolude

import Control.Monad.Random (MonadRandom, Random, getRandom)
import Test.Tasty.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Cyclic groups.
class (Arbitrary g, Eq g, Generic g, Monoid g,
       NFData g, Pretty g, Random g, Show g) => Cyclic g where
  {-# MINIMAL add, dbl, def, gen, id, inv, order #-}

  -- | Element addition.
  add :: g -> g -> g

  -- | Element doubling.
  dbl :: g -> g

  -- | Check well-defined.
  def :: g -> Bool

  -- | Cyclic group generator.
  gen :: g

  -- | Identity element.
  id :: g

  -- | Element inversion.
  inv :: g -> g

  -- | Element multiplication.
  mul' :: g -> Integer -> g
  mul' p n
    | n < 0     = inv (mul' p (-n))
    | n == 0    = id
    | n == 1    = p
    | even n    = p'
    | otherwise = add p p'
    where
      p' = mul' (dbl p) (div n 2)
  {-# INLINABLE mul' #-}

  -- | Curve order.
  order :: g -> Integer

  -- | Random element.
  rnd :: MonadRandom m => m g
  rnd = getRandom
  {-# INLINABLE rnd #-}
