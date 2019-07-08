module Curve.BinaryWeierstrass.SECT233R1
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

import BinaryField (BinaryField)

import Curve.BinaryWeierstrass (BWCurve(..), BWPoint, Point(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SECT233R1 curve
data SECT233R1

-- | Field of SECT233R1 curve
type F2m = BinaryField 0x20000000000000000000000000000000000000004000000000000000001

-- | SECT233R1 curve is a binary Weierstrass curve
instance BWCurve SECT233R1 0x20000000000000000000000000000000000000004000000000000000001 where
  a_ = const _a 
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT233R1 curve
type P = BWPoint SECT233R1 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT233R1 curve
_a :: F2m
_a = 1
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT233R1 curve
_b :: F2m
_b = 0x0066647ede6c332c7f8c0923bb58213b333b20e9ce4281fe115f7d8f90ad
{-# INLINE _b #-}

-- | Polynomial of SECT233R1 curve
_f :: Integer
_f = 0x20000000000000000000000000000000000000004000000000000000001
{-# INLINE _f #-}

-- | Generator of SECT233R1 curve
_g :: P
_g = A 0x00fac9dfcbac8313bb2139f1bb755fef65bc391f8b36f8f8eb7371fd558b
       0x01006a08a41903350678e58528bebf8a0beff867a7ca36716f7e01f81052
{-# INLINE _g #-}

-- | Cofactor of SECT233R1 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT233R1 curve
_n :: Integer
_n = 0x01000000000000000000000000000013e974e72f8a6922031d2603cfe0d7
{-# INLINE _n #-}
