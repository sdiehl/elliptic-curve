module Curve.BinaryWeierstrass.SECT163R1
  -- | Types
  ( F2m
  , P
  -- | Parameters
  , _a
  , _b
  , _f
  , _g
  , _h
  , _n
  ) where

import Protolude

import ExtensionField (IrreducibleMonic(..), x)

import Curve.BinaryWeierstrass (BWCurve(..), BWPoint, F2, Fm, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT163R1 curve
data SECT163R1

-- | Field of SECT163R1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (163 :: Int) + x ^ (7 :: Int) + x ^ (6 :: Int) + x ^ (3 :: Int) + 1
type F2m = Fm FX

-- | SECT163R1 curve is a binary Weierstrass curve
instance BWCurve SECT163R1 FX where
  a_ = const _a 
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT163R1 curve
type P = BWPoint SECT163R1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT163R1 curve
_a :: F2m
_a = 0x07b6882caaefa84f9554ff8428bd88e246d2782ae2
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT163R1 curve
_b :: F2m
_b = 0x0713612dcddcb40aab946bda29ca91f73af958afd9
{-# INLINE _b #-}

-- | Polynomial of SECT163R1 curve
_f = split (witness :: F2m)
{-# INLINE _f #-}

-- | Generator of SECT163R1 curve
_g :: P
_g = A 0x0369979697ab43897789566789567f787a7876a654
       0x00435edb42efafb2989d51fefce3c80988f41ff883
{-# INLINE _g #-}

-- | Cofactor of SECT163R1 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT163R1 curve
_n :: Integer
_n = 0x03ffffffffffffffffffff48aab689c29ca710279b
{-# INLINE _n #-}
