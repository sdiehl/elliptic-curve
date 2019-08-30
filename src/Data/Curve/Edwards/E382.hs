module Data.Curve.Edwards.E382
  ( module Data.Curve.Edwards
  -- * E382 curve
  , module Data.Curve.Edwards.E382
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Edwards
import Data.Curve.Edwards.Base (ECurve(..), EACurve(..), EPCurve(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | E382 curve.
data E382

-- | Field of points of E382 curve.
type Fq = Prime 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff97

-- | Field of coefficients of E382 curve.
type Fr = Prime 0xfffffffffffffffffffffffffffffffffffffffffffffffd5fb21f21e95eee17c5e69281b102d2773e27e13fd3c9719

-- E382 curve is an Edwards curve.
instance Curve 'Edwards c E382 Fq Fr => ECurve c E382 Fq Fr where
  a_ = const _a
  {-# INLINABLE a_ #-}
  d_ = const _d
  {-# INLINABLE d_ #-}
  h_ = const _h
  {-# INLINABLE h_ #-}
  q_ = const _q
  {-# INLINABLE q_ #-}
  r_ = const _r
  {-# INLINABLE r_ #-}

-- | Affine E382 curve point.
type PA = EAPoint E382 Fq Fr

-- Affine E382 curve is an Edwards affine curve.
instance EACurve E382 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective E382 point.
type PP = EPPoint E382 Fq Fr

-- Projective E382 curve is an Edwards projective curve.
instance EPCurve E382 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of E382 curve.
_a :: Fq
_a = 0x1
{-# INLINABLE _a #-}

-- | Coefficient @D@ of E382 curve.
_d :: Fq
_d = 0x3ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffef8e1
{-# INLINABLE _d #-}

-- | Cofactor of E382 curve.
_h :: Natural
_h = 0x4
{-# INLINABLE _h #-}

-- | Characteristic of E382 curve.
_q :: Natural
_q = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff97
{-# INLINABLE _q #-}

-- | Order of E382 curve.
_r :: Natural
_r = 0xfffffffffffffffffffffffffffffffffffffffffffffffd5fb21f21e95eee17c5e69281b102d2773e27e13fd3c9719
{-# INLINABLE _r #-}

-- | Coordinate @X@ of E382 curve.
_x :: Fq
_x = 0x196f8dd0eab20391e5f05be96e8d20ae68f840032b0b64352923bab85364841193517dbce8105398ebc0cc9470f79603
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of E382 curve.
_y :: Fq
_y = 0x11
{-# INLINABLE _y #-}

-- | Generator of affine E382 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective E382 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
