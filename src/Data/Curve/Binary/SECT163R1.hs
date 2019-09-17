module Data.Curve.Binary.SECT163R1
  ( module Data.Curve.Binary
  -- * SECT163R1 curve
  , module Data.Curve.Binary.SECT163R1
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT163R1 curve.
data SECT163R1

-- | Field of points of SECT163R1 curve.
type F2m = Binary P
type P = 0x800000000000000000000000000000000000000c9

-- | Field of coefficients of SECT163R1 curve.
type Fr = Prime R
type R = 0x3ffffffffffffffffffff48aab689c29ca710279b

-- SECT163R1 curve is a binary curve.
instance Curve 'Binary c SECT163R1 F2m Fr => BCurve c SECT163R1 F2m Fr where
  a_ = const _a
  {-# INLINABLE a_ #-}
  b_ = const _b
  {-# INLINABLE b_ #-}
  h_ = const _h
  {-# INLINABLE h_ #-}
  p_ = const _p
  {-# INLINABLE p_ #-}
  r_ = const _r
  {-# INLINABLE r_ #-}

-- | Affine SECT163R1 curve point.
type PA = BAPoint SECT163R1 F2m Fr

-- Affine SECT163R1 curve is a binary affine curve.
instance BACurve SECT163R1 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT163R1 point.
type PP = BPPoint SECT163R1 F2m Fr

-- Projective SECT163R1 curve is a binary projective curve.
instance BPCurve SECT163R1 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT163R1 curve.
_a :: F2m
_a = 0x7b6882caaefa84f9554ff8428bd88e246d2782ae2
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT163R1 curve.
_b :: F2m
_b = 0x713612dcddcb40aab946bda29ca91f73af958afd9
{-# INLINABLE _b #-}

-- | Cofactor of SECT163R1 curve.
_h :: Natural
_h = 0x2
{-# INLINABLE _h #-}

-- | Polynomial of SECT163R1 curve.
_p :: Natural
_p = 0x800000000000000000000000000000000000000c9
{-# INLINABLE _p #-}

-- | Order of SECT163R1 curve.
_r :: Natural
_r = 0x3ffffffffffffffffffff48aab689c29ca710279b
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT163R1 curve.
_x :: F2m
_x = 0x369979697ab43897789566789567f787a7876a654
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT163R1 curve.
_y :: F2m
_y = 0x435edb42efafb2989d51fefce3c80988f41ff883
{-# INLINABLE _y #-}

-- | Generator of affine SECT163R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT163R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
