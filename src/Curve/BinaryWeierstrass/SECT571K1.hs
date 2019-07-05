module Curve.BinaryWeierstrass.SECT571K1
  -- | Imports
  ( BWCurve(..)
  , BWPoint
  , F2
  , Fm
  , Point(..)
  -- | Types
  , F2m
  , P
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
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 1
  {-# INLINE _b #-}
  _f _ = witness :: FX
  {-# INLINE _f #-}
  _g   = A 0x026eb7a859923fbc82189631f8103fe4ac9ca2970012d5d46024804801841ca44370958493b205e647da304db4ceb08cbbd1ba39494776fb988b47174dca88c7e2945283a01c8972
           0x0349dc807f4fbf374f4aeade3bca95314dd58cec9f307a54ffc61efc006d8a2c9d4979c0ac44aea74fbebbb9f772aedcb620b01a7ba7af1b320430c8591984f601cd4c143ef1c7a3
  {-# INLINE _g #-}
  _h _ = 4
  {-# INLINE _h #-}
  _n _ = 0x020000000000000000000000000000000000000000000000000000000000000000000000131850e1f19a63e4b391a8db917f4138b630d84be5d639381e91deb45cfe778f637c1001
  {-# INLINE _n #-}

-- | Point of SECT571K1 curve
type P = BWPoint SECT571K1 F2m
