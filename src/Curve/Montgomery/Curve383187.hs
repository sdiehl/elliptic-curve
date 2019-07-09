module Curve.Montgomery.Curve383187
  -- | Types
  ( Fp
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

import PrimeField (PrimeField)

import Curve.Montgomery (Point(..), MCurve(..), MPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Curve383187 curve
data Curve383187

-- | Field of Curve383187 curve
type Fp = PrimeField 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45  

-- | Curve383187 curve is a Montgomery curve
instance MCurve Curve383187 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of Curve383187 curve
type P = MPoint Curve383187 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Curve383187 curve
_a :: Fp
_a = 0x38251
{-# INLINE _a #-}

-- | Coefficient @B@ of Curve383187 curve
_b :: Fp
_b = 1
{-# INLINE _b #-}

-- | Generator of Curve383187 curve
_g :: P
_g = A
     5
     0x1eebe07dc1871896732b12d5504a32370471965c7a11f2c89865f855ab3cbd7c224e3620c31af3370788457dd5ce46df
{-# INLINE _g #-}

-- | Cofactor of Curve383187 curve
_h :: Integer
_h = 8
{-# INLINE _h #-}

-- | Order of Curve383187 curve
_n :: Integer
_n = notImplemented
{-# INLINE _n #-}

-- | Characteristic of Curve383187 curve
_p :: Integer
_p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff45 
{-# INLINE _p #-}
