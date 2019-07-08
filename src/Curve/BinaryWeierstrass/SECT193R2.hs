module Curve.BinaryWeierstrass.SECT193R2
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

-- | SECT193R2 curve
data SECT193R2

-- | Field of SECT193R2 curve
type F2m = BinaryField 0x2000000000000000000000000000000000000000000008001

-- | SECT193R2 curve is a binary Weierstrass curve
instance BWCurve SECT193R2 0x2000000000000000000000000000000000000000000008001 where
  a_ = const _a 
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}

-- | Point of SECT193R2 curve
type P = BWPoint SECT193R2 F2m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of SECT193R2 curve
_a :: F2m
_a = 0x0163f35a5137c2ce3ea6ed8667190b0bc43ecd69977702709b
{-# INLINE _a #-}

-- | Coefficient @B@ of SECT193R2 curve
_b :: F2m
_b = 0x00c9bb9e8927d4d64c377e2ab2856a5b16e3efb7f61d4316ae
{-# INLINE _b #-}

-- | Polynomial of SECT193R2 curve
_f :: Integer
_f = 0x2000000000000000000000000000000000000000000008001
{-# INLINE _f #-}

-- | Generator of SECT193R2 curve
_g :: P
_g = A 0x00d9b67d192e0367c803f39e1a7e82ca14a651350aae617e8f
       0x01ce94335607c304ac29e7defbd9ca01f596f927224cdecf6c
{-# INLINE _g #-}

-- | Cofactor of SECT193R2 curve
_h :: Integer
_h = 2
{-# INLINE _h #-}

-- | Order of SECT193R2 curve
_n :: Integer
_n = 0x010000000000000000000000015aab561b005413ccd4ee99d5
{-# INLINE _n #-}
