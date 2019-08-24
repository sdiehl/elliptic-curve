module Curve.Weierstrass.BrainpoolP384R1
  ( module Curve.Weierstrass
  , module Curve.Weierstrass.BrainpoolP384R1
  , Point(..)
  ) where

import Protolude

import Data.Field.Galois

import Curve.Weierstrass

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | BrainpoolP384R1 curve.
data BrainpoolP384R1

-- | Field of points of BrainpoolP384R1 curve.
type Fq = Prime 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec53

-- | Field of coefficients of BrainpoolP384R1 curve.
type Fr = Prime 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b31f166e6cac0425a7cf3ab6af6b7fc3103b883202e9046565

-- | BrainpoolP384R1 curve is a Weierstrass curve.
instance Curve 'Weierstrass c BrainpoolP384R1 Fq Fr => WCurve c BrainpoolP384R1 Fq Fr where
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
  x_ = const _x
  {-# INLINABLE x_ #-}
  y_ = const _y
  {-# INLINABLE y_ #-}

-- | Affine BrainpoolP384R1 curve point.
type PA = WAPoint BrainpoolP384R1 Fq Fr

-- | Affine BrainpoolP384R1 curve is a Weierstrass affine curve.
instance WACurve BrainpoolP384R1 Fq Fr where
  gA_ = gA
  {-# INLINABLE gA_ #-}

-- | Jacobian BrainpoolP384R1 point.
type PJ = WJPoint BrainpoolP384R1 Fq Fr

-- | Jacobian BrainpoolP384R1 curve is a Weierstrass Jacobian curve.
instance WJCurve BrainpoolP384R1 Fq Fr where
  gJ_ = gJ
  {-# INLINABLE gJ_ #-}

-- | Projective BrainpoolP384R1 point.
type PP = WPPoint BrainpoolP384R1 Fq Fr

-- | Projective BrainpoolP384R1 curve is a Weierstrass projective curve.
instance WPCurve BrainpoolP384R1 Fq Fr where
  gP_ = gP
  {-# INLINABLE gP_ #-}

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Coefficient @A@ of BrainpoolP384R1 curve.
_a :: Fq
_a = 0x7bc382c63d8c150c3c72080ace05afa0c2bea28e4fb22787139165efba91f90f8aa5814a503ad4eb04a8c7dd22ce2826
{-# INLINABLE _a #-}

-- | Coefficient @B@ of BrainpoolP384R1 curve.
_b :: Fq
_b = 0x4a8c7dd22ce28268b39b55416f0447c2fb77de107dcd2a62e880ea53eeb62d57cb4390295dbc9943ab78696fa504c11
{-# INLINABLE _b #-}

-- | Cofactor of BrainpoolP384R1 curve.
_h :: Integer
_h = 0x1
{-# INLINABLE _h #-}

-- | Characteristic of BrainpoolP384R1 curve.
_q :: Integer
_q = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b412b1da197fb71123acd3a729901d1a71874700133107ec53
{-# INLINABLE _q #-}

-- | Order of BrainpoolP384R1 curve.
_r :: Integer
_r = 0x8cb91e82a3386d280f5d6f7e50e641df152f7109ed5456b31f166e6cac0425a7cf3ab6af6b7fc3103b883202e9046565
{-# INLINABLE _r #-}

-- | Coordinate @X@ of BrainpoolP384R1 curve.
_x :: Fq
_x = 0x1d1c64f068cf45ffa2a63a81b7c13f6b8847a3e77ef14fe3db7fcafe0cbd10e8e826e03436d646aaef87b2e247d4af1e
{-# INLINABLE _x #-}

-- | Coordinate @Y@ of BrainpoolP384R1 curve.
_y :: Fq
_y = 0x8abe1d7520f9c2a45cb1eb8e95cfd55262b70b29feec5864e19c054ff99129280e4646217791811142820341263c5315
{-# INLINABLE _y #-}

-- | Generator of affine BrainpoolP384R1 curve.
gA :: PA
gA = A _x _y
{-# INLINABLE gA #-}

-- | Generator of Jacobian BrainpoolP384R1 curve.
gJ :: PJ
gJ = J _x _y 1
{-# INLINABLE gJ #-}

-- | Generator of projective BrainpoolP384R1 curve.
gP :: PP
gP = P _x _y 1
{-# INLINABLE gP #-}
