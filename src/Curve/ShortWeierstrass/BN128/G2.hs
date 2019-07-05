module Curve.ShortWeierstrass.BN128.G2
  -- | Imports
  ( Point(..)
  , SWCurve(..)
  , SWPoint
  -- | Types
  , Fp2
  , P
  ) where

import Protolude

import ExtensionField (ExtensionField, IrreducibleMonic(..), fromList, x)

import Curve.ShortWeierstrass (Point(..), SWCurve(..), SWPoint)
import Curve.ShortWeierstrass.BN128.G1 (Fp)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN128 curve G2
data G2

-- | Field of BN128 curve G2
data PolynomialU
instance IrreducibleMonic Fp PolynomialU where
  split _ = x ^ (2 :: Int) + 1
type Fp2 = ExtensionField Fp PolynomialU

-- | BN128 curve G2 is a short Weierstrass curve
instance SWCurve G2 Fp2 where
  _a _ = 0
  {-# INLINE _a #-}
  _b _ = 3 / fromList [9, 1]
  {-# INLINE _b #-}
  _g   = A
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
  _h _ = notImplemented
  {-# INLINE _h #-}
  _n _ = notImplemented
  {-# INLINE _n #-}
  _p _ = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
  {-# INLINE _p #-}

-- | Point of BN128 curve G2
type P = SWPoint G2 Fp2
