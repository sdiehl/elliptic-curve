module Curve.Weierstrass.BN462
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , PA
  , PP
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , WPCurve(..)
  , WPPoint
  , _a
  , _b
  , _h
  , _q
  , _r
  , _x
  , _y
  , gA
  , gP
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN462 curve.
data BN462

-- | Field of points of BN462 curve.
type Fq = PrimeField 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908f41c8020ffffffffff6ff66fc6ff687f640000000002401b00840138013

-- | Field of coefficients of BN462 curve.
type Fr = PrimeField 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908ee1c201f7fffffffff6ff66fc7bf717f7c0000000002401b007e010800d

-- | BN462 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN462 Fq => WCurve c BN462 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine BN462 curve point.
type PA = WAPoint BN462 Fq

-- | Affine BN462 curve is a Weierstrass affine curve.
instance WACurve BN462 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Projective BN462 point.
type PP = WPPoint BN462 Fq

-- | Projective BN462 curve is a Weierstrass projective curve.
instance WPCurve BN462 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN462 curve.
_a :: Fq
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN462 curve.
_b :: Fq
_b = 0x5
{-# INLINE _b #-}

-- | Cofactor of BN462 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BN462 curve.
_q :: Integer
_q = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908f41c8020ffffffffff6ff66fc6ff687f640000000002401b00840138013
{-# INLINE _q #-}

-- | Order of BN462 curve.
_r :: Integer
_r = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908ee1c201f7fffffffff6ff66fc7bf717f7c0000000002401b007e010800d
{-# INLINE _r #-}

-- | Coordinate @X@ of BN462 curve.
_x :: Fq
_x = 0x21a6d67ef250191fadba34a0a30160b9ac9264b6f95f63b3edbec3cf4b2e689db1bbb4e69a416a0b1e79239c0372e5cd70113c98d91f36b6980d
{-# INLINE _x #-}

-- | Coordinate @Y@ of BN462 curve.
_y :: Fq
_y = 0x118ea0460f7f7abb82b33676a7432a490eeda842cccfa7d788c659650426e6af77df11b8ae40eb80f475432c66600622ecaa8a5734d36fb03de
{-# INLINE _y #-}

-- | Affine generator of BN462 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Projective generator of BN462 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
