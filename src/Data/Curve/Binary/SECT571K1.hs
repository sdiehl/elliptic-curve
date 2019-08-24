module Data.Curve.Binary.SECT571K1
  ( module Data.Curve.Binary
  , module Data.Curve.Binary.SECT571K1
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Data.Curve.Binary

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT571K1 curve.
data SECT571K1

-- | Field of points of SECT571K1 curve.
type F2m = Binary 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425

-- | Field of coefficients of SECT571K1 curve.
type Fr = Prime 0x20000000000000000000000000000000000000000000000000000000000000000000000131850e1f19a63e4b391a8db917f4138b630d84be5d639381e91deb45cfe778f637c1001

-- | SECT571K1 curve is a binary curve.
instance Curve 'Binary c SECT571K1 F2m Fr => BCurve c SECT571K1 F2m Fr where
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
  x_ = const _x
  {-# INLINABLE x_ #-}
  y_ = const _y
  {-# INLINABLE y_ #-}

-- | Affine SECT571K1 curve point.
type PA = BAPoint SECT571K1 F2m Fr

-- | Affine SECT571K1 curve is a binary affine curve.
instance BACurve SECT571K1 F2m Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Projective SECT571K1 point.
type PP = BPPoint SECT571K1 F2m Fr

-- | Projective SECT571K1 curve is a binary projective curve.
instance BPCurve SECT571K1 F2m Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT571K1 curve.
_a :: F2m
_a = 0x0
{-# INLINABLE _a #-}

-- | Coefficient @B@ of SECT571K1 curve.
_b :: F2m
_b = 0x1
{-# INLINABLE _b #-}

-- | Cofactor of SECT571K1 curve.
_h :: Integer
_h = 0x4
{-# INLINABLE _h #-}

-- | Polynomial of SECT571K1 curve.
_p :: Integer
_p = 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425
{-# INLINABLE _p #-}

-- | Order of SECT571K1 curve.
_r :: Integer
_r = 0x20000000000000000000000000000000000000000000000000000000000000000000000131850e1f19a63e4b391a8db917f4138b630d84be5d639381e91deb45cfe778f637c1001
{-# INLINABLE _r #-}

-- | Coordinate @X@ of SECT571K1 curve.
_x :: F2m
_x = 0x26eb7a859923fbc82189631f8103fe4ac9ca2970012d5d46024804801841ca44370958493b205e647da304db4ceb08cbbd1ba39494776fb988b47174dca88c7e2945283a01c8972
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of SECT571K1 curve.
_y :: F2m
_y = 0x349dc807f4fbf374f4aeade3bca95314dd58cec9f307a54ffc61efc006d8a2c9d4979c0ac44aea74fbebbb9f772aedcb620b01a7ba7af1b320430c8591984f601cd4c143ef1c7a3
{-# INLINABLE _y #-}

-- | Generator of affine SECT571K1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of projective SECT571K1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
