module Curve.Binary.SECT163R1
  ( AP
  , BCurve(..)
  , BPoint
  , BACurve(..)
  , BAPoint
  , Curve(..)
  , F2m
  , Fr
  , Group(..)
  , Point(..)
  , _a
  , _b
  , _g
  , _h
  , _p
  , _r
  , _x
  , _y
  ) where

import Protolude

import BinaryField (BinaryField)
import PrimeField (PrimeField)

import Curve (Curve(..), Form(..))
import Curve.Binary (BCurve(..), BPoint, BACurve(..), BAPoint, Point(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- SECT163R1 curve
-------------------------------------------------------------------------------

-- | SECT163R1 curve.
data SECT163R1

-- | Field of points of SECT163R1 curve.
type F2m = BinaryField 0x800000000000000000000000000000000000000c9

-- | Field of coefficients of SECT163R1 curve.
type Fr = PrimeField 0x3ffffffffffffffffffff48aab689c29ca710279b

-- | SECT163R1 curve is a binary curve.
instance Curve 'Binary c SECT163R1 F2m => BCurve c SECT163R1 F2m where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  p_ = const _p
  {-# INLINE p_ #-}
  r_ = const _r
  {-# INLINE r_ #-}

-- | Coefficient @A@ of SECT163R1 curve.
_a :: F2m
_a = 0x7b6882caaefa84f9554ff8428bd88e246d2782ae2
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT163R1 curve.
_b :: F2m
_b = 0x713612dcddcb40aab946bda29ca91f73af958afd9
{-# INLINE _b #-}

-- | Cofactor of SECT163R1 curve.
_h :: Integer
_h = 0x2
{-# INLINE _h #-}

-- | Polynomial of SECT163R1 curve.
_p :: Integer
_p = 0x800000000000000000000000000000000000000c9
{-# INLINE _p #-}

-- | Order of SECT163R1 curve.
_r :: Integer
_r = 0x3ffffffffffffffffffff48aab689c29ca710279b
{-# INLINE _r #-}

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Affine SECT163R1 point.
type AP = BAPoint SECT163R1 F2m

-- | Affine SECT163R1 curve is a binary affine curve.
instance BACurve SECT163R1 F2m where
  g_ = _g
  {-# INLINE g_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Generator of affine SECT163R1 curve.
_g :: AP
_g = A _x _y
{-# INLINE _g #-}

-- | Coordinate @X@ of affine SECT163R1 curve.
_x :: F2m
_x = 0x369979697ab43897789566789567f787a7876a654
{-# INLINE _x #-}

-- | Coordinate @Y@ of affine SECT163R1 curve.
_y :: F2m
_y = 0x435edb42efafb2989d51fefce3c80988f41ff883
{-# INLINE _y #-}
