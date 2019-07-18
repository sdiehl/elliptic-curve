module Curve.Weierstrass.BN462T
  -- | Types
  ( Fp2
  , P
  -- | Parameters
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic(..), fromList)

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Curve.Weierstrass.BN462 (Fp)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN462T curve
data BN462T

-- | Field of BN462T curve
data PolynomialU
instance IrreducibleMonic Fp PolynomialU where
  split _ = [1, 0, 1]
type Fp2 = ExtensionField Fp PolynomialU

-- | BN462T curve is a Weierstrass curve
instance WCurve BN462T Fp2 where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of BN462T curve
type P = WPoint BN462T Fp2

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN462T curve
_a :: Fp2
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN462T curve
_b :: Fp2
_b = fromList [2, -1]
{-# INLINE _b #-}

-- | Generator of BN462T curve
_g :: P
_g = A
  ( fromList
   [ 0x257ccc85b58dda0dfb38e3a8cbdc5482e0337e7c1cd96ed61c913820408208f9ad2699bad92e0032ae1f0aa6a8b48807695468e3d934ae1e4df
   , 0x1d2e4343e8599102af8edca849566ba3c98e2a354730cbed9176884058b18134dd86bae555b783718f50af8b59bf7e850e9b73108ba6aa8cd283
   ]
  )
  ( fromList
   [ 0xa0650439da22c1979517427a20809eca035634706e23c3fa7a6bb42fe810f1399a1f41c9ddae32e03695a140e7b11d7c3376e5b68df0db7154e
   , 0x73ef0cbd438cbe0172c8ae37306324d44d5e6b0c69ac57b393f1ab370fd725cc647692444a04ef87387aa68d53743493b9eba14cc552ca2a93a
   ]
  )
{-# INLINE _g #-}

-- | Cofactor of BN462T curve
_h :: Integer
_h = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908fa1ce0227fffffffff6ff66fc63f5f7f4c0000000002401b008a0168019
{-# INLINE _h #-}

-- | Order of BN462T curve
_n :: Integer
_n = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908ee1c201f7fffffffff6ff66fc7bf717f7c0000000002401b007e010800d
{-# INLINE _n #-}

-- | Characteristic of BN462T curve
_p :: Integer
_p = 0x240480360120023ffffffffff6ff0cf6b7d9bfca0000000000d812908f41c8020ffffffffff6ff66fc6ff687f640000000002401b00840138013
{-# INLINE _p #-}
