module Curve.Weierstrass.BN254AT
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

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)
import Curve.Weierstrass.BN254A (Fp)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BN254AT curve
data BN254AT

-- | Field of BN254AT curve
data PolynomialU
instance IrreducibleMonic Fp PolynomialU where
  split _ = x ^ (2 :: Int) + 5
type Fp2 = ExtensionField Fp PolynomialU

-- | BN254AT curve is a Weierstrass curve
instance WCurve BN254AT Fp2 where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of BN254AT curve
type P = WPoint BN254AT Fp2

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254AT curve
_a :: Fp2
_a = 0
{-# INLINE _a #-}

-- | Coefficient @B@ of BN254AT curve
_b :: Fp2
_b = fromList [0, -1]
{-# INLINE _b #-}

-- | Generator of BN254AT curve
_g :: P
_g = A
  ( fromList
   [ 0x19b0bea4afe4c330da93cc3533da38a9f430b471c6f8a536e81962ed967909b5
   , 0xa1cf585585a61c6e9880b1f2a5c539f7d906fff238fa6341e1de1a2e45c3f72
   ]
  )
  ( fromList
   [ 0x17abd366ebbd65333e49c711a80a0cf6d24adf1b9b3990eedcc91731384d2627
   , 0xee97d6de9902a27d00e952232a78700863bc9aa9be960C32f5bf9fd0a32d345
   ]
  )
{-# INLINE _g #-}

-- | Cofactor of BN254AT curve
_h :: Integer
_h = 0x2370fb049d410fbe4e761a9886e50241dc42cf101e0000017e80600000000001
{-# INLINE _h #-}

-- | Order of BN254AT curve
_n :: Integer
_n = 0x2370fb049d410fbe4e761a9886e502411dc1af70120000017e80600000000001
{-# INLINE _n #-}

-- | Characteristic of BN254AT curve
_p :: Integer
_p = 0x2370fb049d410fbe4e761a9886e502417d023f40180000017e80600000000001
{-# INLINE _p #-}
