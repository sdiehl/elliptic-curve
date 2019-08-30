module Data.Curve.Montgomery.Curve448
  ( module Data.Curve.Montgomery
  -- * Curve448 curve
  , module Data.Curve.Montgomery.Curve448
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Montgomery
import Data.Curve.Montgomery.Base (MCurve(..), MACurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Curve448 curve.
data Curve448

-- | Field of points of Curve448 curve.
type Fq = Prime 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff

-- | Field of coefficients of Curve448 curve.
type Fr = Prime 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3

-- Curve448 curve is a Montgomery curve.
instance Curve 'Montgomery c Curve448 Fq Fr => MCurve c Curve448 Fq Fr where
  a_ = const _a
  {-# INLINABLE a_ #-}
  b_ = const _b
  {-# INLINABLE b_ #-}
  h_ = const _h
  {-# INLINABLE h_ #-}
  q_ = const _q
  {-# INLINABLE q_ #-}
  r_ = const _r
  {-# INLINABLE r_ #-}

-- | Affine Curve448 curve point.
type PA = MAPoint Curve448 Fq Fr

-- Affine Curve448 curve is a Montgomery affine curve.
instance MACurve Curve448 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Curve448 curve.
_a :: Fq
_a = 0x262a6
{-# INLINABLE _a #-}

-- | Coefficient @B@ of Curve448 curve.
_b :: Fq
_b = 0x1
{-# INLINABLE _b #-}

-- | Cofactor of Curve448 curve.
_h :: Natural
_h = 0x4
{-# INLINABLE _h #-}

-- | Characteristic of Curve448 curve.
_q :: Natural
_q = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffff
{-# INLINABLE _q #-}

-- | Order of Curve448 curve.
_r :: Natural
_r = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffff7cca23e9c44edb49aed63690216cc2728dc58f552378c292ab5844f3
{-# INLINABLE _r #-}

-- | Coordinate @X@ of Curve448 curve.
_x :: Fq
_x = 0x5
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of Curve448 curve.
_y :: Fq
_y = 0x7d235d1295f5b1f66c98ab6e58326fcecbae5d34f55545d060f75dc28df3f6edb8027e2346430d211312c4b150677af76fd7223d457b5b1a
{-# INLINABLE _y #-}

-- | Generator of affine Curve448 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}
