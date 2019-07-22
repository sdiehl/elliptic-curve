module Curve.Weierstrass.BrainpoolP192R1
  ( Fp
  , P
  , _a
  , _b
  , _g
  , _h
  , _n
  , _p
  ) where

import Protolude

import PrimeField (PrimeField)

import Curve.Weierstrass (Point(..), WCurve(..), WPoint)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Brainpool-P192R1 curve
data BrainpoolP192R1

-- | Field of Brainpool-P192R1 curve
type Fp = PrimeField 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297

-- | Brainpool-P192R1 curve is a Weierstrass curve
instance WCurve BrainpoolP192R1 Fp where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  g_ = _g
  {-# INLINE g_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  n_ = const _n
  {-# INLINE n_ #-}
  p_ = const _p
  {-# INLINE p_ #-}

-- | Point of Brainpool-P192R1 curve
type P = WPoint BrainpoolP192R1 Fp

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of Brainpool-P192R1 curve
_a :: Fp
_a = 0x6a91174076b1e0e19c39c031fe8685c1cae040e5c69a28ef
{-# INLINE _a #-}

-- | Coefficient @B@ of Brainpool-P192R1 curve
_b :: Fp
_b = 0x469a28ef7c28cca3dc721d044f4496bcca7ef4146fbf25c9
{-# INLINE _b #-}

-- | Generator of Brainpool-P192R1 curve
_g :: P
_g = A
     0xc0a0647eaab6a48753b033c56cb0f0900a2f5c4853375fd6
     0x14b690866abd5bb88b5f4828c1490002e6773fa2fa299b8f
{-# INLINE _g #-}

-- | Cofactor of Brainpool-P192R1 curve
_h :: Integer
_h = 1
{-# INLINE _h #-}

-- | Order of Brainpool-P192R1 curve
_n :: Integer
_n = 0xc302f41d932a36cda7a3462f9e9e916b5be8f1029ac4acc1
{-# INLINE _n #-}

-- | Characteristic of Brainpool-P192R1 curve
_p :: Integer
_p = 0xc302f41d932a36cda7a3463093d18db78fce476de1a86297
{-# INLINE _p #-}
