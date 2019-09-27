module Data.Curve.Weierstrass.BN254DT
  ( module Data.Curve.Weierstrass
  , Point(..)
  -- * BN254D curve
  , module Data.Curve.Weierstrass.BN254DT
  ) where

import Protolude

import Data.Field.Galois
import GHC.Natural (Natural)

import Data.Curve.Weierstrass
import Data.Curve.Weierstrass.BN254D (BN254D, Fq, Fr)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Field of points of BN254D curve over @Fq2@.
type Fq2 = Extension U Fq
data U
instance IrreducibleMonic U Fq where
  poly _ = [1, 0, 1]
  {-# INLINABLE poly #-}

-- BN254D curve is a Weierstrass curve.
instance Curve 'Weierstrass c BN254D Fq2 Fr => WCurve c BN254D Fq2 Fr where
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

-- | Affine BN254D curve point.
type PA = WAPoint BN254D Fq2 Fr

-- Affine BN254D curve is a Weierstrass affine curve.
instance WACurve BN254D Fq2 Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BN254D point.
type PJ = WJPoint BN254D Fq2 Fr

-- Jacobian BN254D curve is a Weierstrass Jacobian curve.
instance WJCurve BN254D Fq2 Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BN254D point.
type PP = WPPoint BN254D Fq2 Fr

-- Projective BN254D curve is a Weierstrass projective curve.
instance WPCurve BN254D Fq2 Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BN254D curve.
_a :: Fq2
_a = toE' [
          ]
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BN254D curve.
_b :: Fq2
_b = toE' [ 0x1
          , 0x24000482410f5aadb74e200f3b89d00081cf93e428f0d651e8b2dc2bb460a48a
          ]
{-# INLINABLE _b #-}

-- | Cofactor of BN254D curve.
_h :: Natural
_h = 0x24000482410f5aadb74e200f3b89d000e1cf99e72a2a746ff96a46b257171d21
{-# INLINABLE _h #-}

-- | Characteristic of BN254D curve.
_q :: Natural
_q = 0x24000482410f5aadb74e200f3b89d00081cf93e428f0d651e8b2dc2bb460a48b
{-# INLINABLE _q #-}

-- | Order of BN254D curve.
_r :: Natural
_r = 0x24000482410f5aadb74e200f3b89d00021cf8de127b73833d7fb71a511aa2bf5
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BN254D curve.
_x :: Fq2
_x = toE' [ 0x20cfe8b965fc444008a21b12cd2a55f843c1dd68ba12a8bb1f1dde3533b91a32
          , 0x176f822a5ee7ada449f8f876ee001508dd43b5413e03c8f4ad3e3b38dadaf51
          ]
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BN254D curve.
_y :: Fq2
_y = toE' [ 0x2b27f22c2920fee3b4af218b6d92421780a9bdc66155142fecef3af7f58e872
          , 0x14e9c62a36ebce710810576b5401fdf0b28126ad2d563bf5043be3347646dfb4
          ]
{-# INLINABLE _y #-}

-- | Generator of affine BN254D curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BN254D curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BN254D curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
