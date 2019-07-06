module Curve.BinaryWeierstrass.SECT571K1
  -- | Types
  ( F2m
  , P
  -- | Parameters
  , _a
  , _b
  , _f
  , _g
  , _h
  , _n
  ) where

import Protolude

import ExtensionField (IrreducibleMonic(..), x)

import Curve.BinaryWeierstrass (BWCurve(..), BWPoint, F2, Fm, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT571K1 curve
data SECT571K1

-- | Field of SECT571K1 curve
data FX
instance IrreducibleMonic F2 FX where
  split _ = x ^ (571 :: Int) + x ^ (10 :: Int) + x ^ (5 :: Int) + x ^ (2 :: Int) + 1
type F2m = Fm FX

-- | SECT571K1 curve is a binary Weierstrass curve
instance BWCurve SECT571K1 FX where
  a_ = const _a 
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT571K1 curve
type P = BWPoint SECT571K1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT571K1 curve
_a :: F2m
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT571K1 curve
_b :: F2m
_b = 1
{-# INLINE _b #-}

-- | Polynomial of SECT571K1 curve
_f = split (witness :: F2m)
{-# INLINE _f #-}

-- | Generator of SECT571K1 curve
_g :: P
_g = A 0x026eb7a859923fbc82189631f8103fe4ac9ca2970012d5d46024804801841ca44370958493b205e647da304db4ceb08cbbd1ba39494776fb988b47174dca88c7e2945283a01c8972
       0x0349dc807f4fbf374f4aeade3bca95314dd58cec9f307a54ffc61efc006d8a2c9d4979c0ac44aea74fbebbb9f772aedcb620b01a7ba7af1b320430c8591984f601cd4c143ef1c7a3
{-# INLINE _g #-}

-- | Cofactor of SECT571K1 curve
_h :: Integer
_h = 4
{-# INLINE _h #-}

-- | Order of SECT571K1 curve
_n :: Integer
_n = 0x020000000000000000000000000000000000000000000000000000000000000000000000131850e1f19a63e4b391a8db917f4138b630d84be5d639381e91deb45cfe778f637c1001
{-# INLINE _n #-}
