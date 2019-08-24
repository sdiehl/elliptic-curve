module Curve.Weierstrass.BN462
  ( module Curve.Weierstrass
  , module Curve.Weierstrass.BN462
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN462 curve.
data BN462

-- | Field of points of BN462 curve.
type Fq = Prime 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908f41c8020ffffffffff6ff66fc6ff687f640000000002401b00840138013

-- | Field of coefficients of BN462 curve.
type Fr = Prime 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908ee1c201f7fffffffff6ff66fc7bf717f7c0000000002401b007e010800d

-- | BN462 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN462 Fq Fr => WCurve c BN462 Fq Fr where
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
  x_ = const _x
  {-# INLINABLE x_ #-}
  y_ = const _y
  {-# INLINABLE y_ #-}

-- | Affine BN462 curve point.
type PA = WAPoint BN462 Fq Fr

-- | Affine BN462 curve is a Weierstrass affine curve.
instance WACurve BN462 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BN462 point.
type PJ = WJPoint BN462 Fq Fr

-- | Jacobian BN462 curve is a Weierstrass Jacobian curve.
instance WJCurve BN462 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BN462 point.
type PP = WPPoint BN462 Fq Fr

-- | Projective BN462 curve is a Weierstrass projective curve.
instance WPCurve BN462 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN462 curve.
_a :: Fq
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BN462 curve.
_b :: Fq
_b = 0x5
{-# INLINABLE _b #-}

-- | Cofactor of BN462 curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BN462 curve.
_q :: Integer
_q = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908f41c8020ffffffffff6ff66fc6ff687f640000000002401b00840138013
{-# INLINABLE _q #-}

-- | Order of BN462 curve.
_r :: Integer
_r = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908ee1c201f7fffffffff6ff66fc7bf717f7c0000000002401b007e010800d
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BN462 curve.
_x :: Fq
_x = 0x21a6d67ef250191fadba34a0a30160b9ac9264b6f95f63b3edbec3cf4b2e689db1bbb4e69a416a0b1e79239c0372e5cd70113c98d91f36b6980d
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BN462 curve.
_y :: Fq
_y = 0x118ea0460f7f7abb82b33676a7432a490eeda842cccfa7d788c659650426e6af77df11b8ae40eb80f475432c66600622ecaa8a5734d36fb03de
{-# INLINABLE _y #-}

-- | Generator of affine BN462 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BN462 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BN462 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
