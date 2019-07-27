module Curve.Binary.SECT571K1
  ( F2m
  , P
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

import Curve.Binary (BCurve(..), BPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT571K1 curve.
data SECT571K1

-- | Field of SECT571K1 curve.
type F2m = BinaryField 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425

-- | SECT571K1 curve is a binary curve.
instance BCurve SECT571K1 F2m where
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

-- | Point of SECT571K1 curve.
type P = BPoint SECT571K1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT571K1 curve.
_a :: F2m
_a = 0x0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT571K1 curve.
_b :: F2m
_b = 0x1
{-# INLINE _b #-}

-- | Generator of SECT571K1 curve.
_g :: P
_g = A _x _y
{-# INLINE _g #-}

-- | Cofactor of SECT571K1 curve.
_h :: Integer
_h = 0x4
{-# INLINE _h #-}

-- | Order of SECT571K1 curve.
_n :: Integer
_n = 0x20000000000000000000000000000000000000000000000000000000000000000000000131850e1f19a63e4b391a8db917f4138b630d84be5d639381e91deb45cfe778f637c1001
{-# INLINE _n #-}

-- | Polynomial of SECT571K1 curve.
_p :: Integer
_p = 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425
{-# INLINE _p #-}

-- | Coordinate @X@ of SECT571K1 curve.
_x :: F2m
_x = 0x26eb7a859923fbc82189631f8103fe4ac9ca2970012d5d46024804801841ca44370958493b205e647da304db4ceb08cbbd1ba39494776fb988b47174dca88c7e2945283a01c8972
{-# INLINE _x #-}

-- | Coordinate @Y@ of SECT571K1 curve.
_y :: F2m
_y = 0x349dc807f4fbf374f4aeade3bca95314dd58cec9f307a54ffc61efc006d8a2c9d4979c0ac44aea74fbebbb9f772aedcb620b01a7ba7af1b320430c8591984f601cd4c143ef1c7a3
{-# INLINE _y #-}
