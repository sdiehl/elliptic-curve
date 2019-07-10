module Curve.ShortWeierstrass.BN254P2
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

import ExtensionField (ExtensionField, IrreducibleMonic(..), fromList, x)

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)
import Curve.ShortWeierstrass.BN254P (Fp)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254P2 curve
data BN254P2

-- | Field of BN254P2 curve
data PolynomialU
instance IrreducibleMonic Fp PolynomialU where
  split _ = x ^ (2 :: Int) + 1
type Fp2 = ExtensionField Fp PolynomialU

-- | BN254P2 curve is a short Weierstrass curve
instance SWCurve BN254P2 Fp2 where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of BN254P2 curve
type P = SWPoint BN254P2 Fp2

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254P2 curve
_a :: Fp2
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254P2 curve
_b :: Fp2
_b = 3 / fromList [9, 1]
{-# INLINE _b #-}

-- | Generator of BN254P2 curve
_g :: P
_g = A
  ( fromList
   [ 0x1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed
   , 0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2
   ]
  )
  ( fromList
   [ 0x12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa
   , 0x90689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b
   ]
  )
{-# INLINE _g #-}

-- | Cofactor of BN254P2 curve
_h :: Integer
_h = notImplemented
{-# INLINE _h #-}

-- | Order of BN254P2 curve
_n :: Integer
_n = notImplemented
{-# INLINE _n #-}

-- | Characteristic of BN254P2 curve
_p :: Integer
_p = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
{-# INLINE _p #-}
