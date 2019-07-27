module Curve.Binary.SECT163R1
  ( BCurve(..)
  , BPoint
  , Curve(..)
  , F2m
  , Group(..)
  , P
  , Point(..)
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  , _x
  , _y
  ) where

import Protolude

import BinaryField (BinaryField)

import Curve (Curve(..), Group(..))
import Curve.Binary (BCurve(..), BPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT163R1 curve.
data SECT163R1

-- | Field of SECT163R1 curve.
type F2m = BinaryField 0x800000000000000000000000000000000000000c9

-- | SECT163R1 curve is a binary curve.
instance BCurve SECT163R1 F2m where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Point of SECT163R1 curve.
type P = BPoint SECT163R1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT163R1 curve.
_a :: F2m
_a = 0x7b6882caaefa84f9554ff8428bd88e246d2782ae2
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT163R1 curve.
_b :: F2m
_b = 0x713612dcddcb40aab946bda29ca91f73af958afd9
{-# INLINE _b #-}

-- | Generator of SECT163R1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECT163R1 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Order of SECT163R1 curve.
_n :: Integer
_n = 0x3ffffffffffffffffffff48aab689c29ca710279b
{-# INLINE _n #-}

-- | Polynomial of SECT163R1 curve.
_p :: Integer
_p = 0x800000000000000000000000000000000000000c9
{-# INLINE _p #-}

-- | Coordinate @X@ of SECT163R1 curve.
_x :: F2m
_x = 0x369979697ab43897789566789567f787a7876a654
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT163R1 curve.
_y :: F2m
_y = 0x435edb42efafb2989d51fefce3c80988f41ff883
{-# INLINE _y #-}
