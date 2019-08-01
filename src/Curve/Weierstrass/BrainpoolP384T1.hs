module Curve.Weierstrass.BrainpoolP384T1
  ( Curve(..)
  , Fq
  , Fr
  , Group(..)
  , PA
  , PP
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , WPCurve(..)
  , WPPoint
  , _a
  , _b
  , _h
  , _q
  , _r
  , _x
  , _y
  , gA
  , gJ
  , gP
  ) where

import Protolude

import PrimeField

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP384T1 curve.
data BrainpoolP384T1

-- | Field of points of BrainpoolP384T1 curve.
type Fq = PrimeField 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec53

-- | Field of coefficients of BrainpoolP384T1 curve.
type Fr = PrimeField 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b31f166e6cac0425a7cf3ab6af6b7fc3103b883202e9046565

-- | BrainpoolP384T1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP384T1 Fq => WCurve c BrainpoolP384T1 Fq where
  a_ = const _a
  {-# INLINE a_ #-}
  b_ = const _b
  {-# INLINE b_ #-}
  h_ = const _h
  {-# INLINE h_ #-}
  q_ = const _q
  {-# INLINE q_ #-}
  r_ = const _r
  {-# INLINE r_ #-}
  x_ = const _x
  {-# INLINE x_ #-}
  y_ = const _y
  {-# INLINE y_ #-}

-- | Affine BrainpoolP384T1 curve point.
type PA = WAPoint BrainpoolP384T1 Fq

-- | Affine BrainpoolP384T1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP384T1 Fq where
  gA_ = gA
  {-# INLINE gA_ #-}

-- | Jacobian BrainpoolP384T1 point.
type PJ = WJPoint BrainpoolP384T1 Fq

-- | Jacobian BrainpoolP384T1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP384T1 Fq where
  gJ_ = gJ
  {-# INLINE gJ_ #-}

-- | Projective BrainpoolP384T1 point.
type PP = WPPoint BrainpoolP384T1 Fq

-- | Projective BrainpoolP384T1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP384T1 Fq where
  gP_ = gP
  {-# INLINE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP384T1 curve.
_a :: Fq
_a = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec50
{-# INLINE _a #-}

-- | Coefficient @B@ of BrainpoolP384T1 curve.
_b :: Fq
_b = 0x7f519eada7bda81bd826dba647910f8c4b9346ed8ccdc64e4b1abd11756dce1d2074aa263b88805ced70355a33b471ee
{-# INLINE _b #-}

-- | Cofactor of BrainpoolP384T1 curve.
_h :: Integer
_h = 0x1
{-# INLINE _h #-}

-- | Characteristic of BrainpoolP384T1 curve.
_q :: Integer
_q = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec53
{-# INLINE _q #-}

-- | Order of BrainpoolP384T1 curve.
_r :: Integer
_r = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b31f166e6cac0425a7cf3ab6af6b7fc3103b883202e9046565
{-# INLINE _r #-}

-- | Coordinate @X@ of BrainpoolP384T1 curve.
_x :: Fq
_x = 0x18de98b02db9a306f2afcd7235f72a819b80ab12ebd653172476fecd462aabffc4ff191b946a5f54d8d0aa2f418808cc
{-# INLINE _x #-}

-- | Coordinate @Y@ of BrainpoolP384T1 curve.
_y :: Fq
_y = 0x25ab056962d30651a114afd2755ad336747f93475b7a1fca3b88f2b6a208ccfe469408584dc2b2912675bf5b9e582928
{-# INLINE _y #-}

-- | Generator of affine BrainpoolP384T1 curve.
gA :: PA
gA = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gA #-}

-- | Generator of Jacobian BrainpoolP384T1 curve.
gJ :: PJ
gJ = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gJ #-}

-- | Generator of projective BrainpoolP384T1 curve.
gP :: PP
gP = fromMaybe (panic "not well-defined.") (point _x _y)
{-# INLINE gP #-}
