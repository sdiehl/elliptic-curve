module Curve.Weierstrass.SECP128R1
  ( module Curve.Weierstrass
  , module Curve.Weierstrass.SECP128R1
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECP128R1 curve.
data SECP128R1

-- | Field of points of SECP128R1 curve.
type Fq = Prime 0xfffffffdffffffffffffffffffffffff

-- | Field of coefficients of SECP128R1 curve.
type Fr = Prime 0xfffffffe0000000075a30d1b9038a115

-- | SECP128R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c SECP128R1 Fq Fr => WCurve c SECP128R1 Fq Fr where
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

-- | Affine SECP128R1 curve point.
type PA = WAPoint SECP128R1 Fq Fr

-- | Affine SECP128R1 curve is a Weierstrass affine curve.
instance WACurve SECP128R1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian SECP128R1 point.
type PJ = WJPoint SECP128R1 Fq Fr

-- | Jacobian SECP128R1 curve is a Weierstrass Jacobian curve.
instance WJCurve SECP128R1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective SECP128R1 point.
type PP = WPPoint SECP128R1 Fq Fr

-- | Projective SECP128R1 curve is a Weierstrass projective curve.
instance WPCurve SECP128R1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECP128R1 curve.
_a :: Fq
_a = 0xfffffffdfffffffffffffffffffffffc
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECP128R1 curve.
_b :: Fq
_b = 0xe87579c11079f43dd824993c2cee5ed3
{-# INLINABLE _b #-}

-- | Cofactor of SECP128R1 curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of SECP128R1 curve.
_q :: Integer
_q = 0xfffffffdffffffffffffffffffffffff
{-# INLINABLE _q #-}

-- | Order of SECP128R1 curve.
_r :: Integer
_r = 0xfffffffe0000000075a30d1b9038a115
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECP128R1 curve.
_x :: Fq
_x = 0x161ff7528b899b2d0c28607ca52c5b86
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECP128R1 curve.
_y :: Fq
_y = 0xcf5ac8395bafeb13c02da292dded7a83
{-# INLINABLE _y #-}

-- | Generator of affine SECP128R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian SECP128R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective SECP128R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
