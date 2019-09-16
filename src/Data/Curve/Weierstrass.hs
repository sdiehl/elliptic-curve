{-# OPTIONS -fno-warn-orphans #-}

module Data.Curve.Weierstrass
  ( module Data.Curve
  , Point(..)
  -- * Weierstrass curves
  , WCurve(..)
  , WPoint
  -- ** Weierstrass affine curves
  , WACurve(..)
  , WAPoint
  -- ** Weierstrass Jacobian curves
  , WJCurve(..)
  , WJPoint
  -- ** Weierstrass projective curves
  , WPCurve(..)
  , WPPoint
  ) where

import Protolude

import Data.Field.Galois as F (GaloisField, PrimeField, frob, sr)
import GHC.Natural (Natural)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Curve (Coordinates(..), Curve(..), Form(..))

-------------------------------------------------------------------------------
-- Weierstrass form
-------------------------------------------------------------------------------

-- | Weierstrass points.
type WPoint = Point 'Weierstrass

-- | Weierstrass curves.
class (GaloisField q, PrimeField r, Curve 'Weierstrass c e q r) => WCurve c e q r where
  {-# MINIMAL a_, b_, h_, q_, r_ #-}
  a_ :: WPoint c e q r -> q       -- ^ Coefficient @A@.
  b_ :: WPoint c e q r -> q       -- ^ Coefficient @B@.
  h_ :: WPoint c e q r -> Natural -- ^ Curve cofactor.
  q_ :: WPoint c e q r -> Natural -- ^ Curve characteristic.
  r_ :: WPoint c e q r -> Natural -- ^ Curve order.

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Weierstrass affine points.
type WAPoint = WPoint 'Affine

-- | Weierstrass affine curves @y^2 = x^3 + Ax + B@.
class WCurve 'Affine e q r => WACurve e q r where
  {-# MINIMAL gA_ #-}
  gA_ :: WAPoint e q r -- ^ Curve generator.

-- Weierstrass affine curves are elliptic curves.
instance WACurve e q r => Curve 'Weierstrass 'Affine e q r where

  data instance Point 'Weierstrass 'Affine e q r = A q q -- ^ Affine point.
                                                 | O     -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)

  add p O       = p
  add O q       = q
  add (A x1 y1) (A x2 y2)
    | x1 == x2  = O
    | otherwise = A x3 y3
    where
      l  = (y1 - y2) / (x1 - x2)
      x3 = l * l - x1 - x2
      y3 = l * (x1 - x3) - y1
  {-# INLINABLE add #-}

  char = q_
  {-# INLINABLE char #-}

  cof = h_
  {-# INLINABLE cof #-}

  dbl O         = O
  dbl (A x y)
    | y == 0    = O
    | otherwise = A x' y'
    where
      a  = a_ (witness :: WAPoint e q r)
      xx = x * x
      l  = (xx + xx + xx + a) / (y + y)
      x' = l * l - x - x
      y' = l * (x - x') - y
  {-# INLINABLE dbl #-}

  def O       = True
  def (A x y) = y * y == (x * x + a) * x + b
    where
      a = a_ (witness :: WAPoint e q r)
      b = b_ (witness :: WAPoint e q r)
  {-# INLINABLE def #-}

  disc _ = 4 * a * a * a + 27 * b * b
    where
      a = a_ (witness :: WAPoint e q r)
      b = b_ (witness :: WAPoint e q r)
  {-# INLINABLE disc #-}

  frob O       = O
  frob (A x y) = A (F.frob x) (F.frob y)
  {-# INLINABLE frob #-}

  fromA = identity
  {-# INLINABLE fromA #-}

  gen = gA_
  {-# INLINABLE gen #-}

  id = O
  {-# INLINABLE id #-}

  inv O       = O
  inv (A x y) = A x (-y)
  {-# INLINABLE inv #-}

  order = r_
  {-# INLINABLE order #-}

  point x y = let p = A x y in if def p then Just p else Nothing
  {-# INLINABLE point #-}

  pointX x = A x <$> yX (witness :: WAPoint e q r) x
  {-# INLINABLE pointX #-}

  toA = identity
  {-# INLINABLE toA #-}

  yX _ x = sr (((x * x + a) * x) + b)
    where
      a = a_ (witness :: WAPoint e q r)
      b = b_ (witness :: WAPoint e q r)
  {-# INLINABLE yX #-}

-- Weierstrass affine points are pretty.
instance WACurve e q r => Pretty (WAPoint e q r) where

  pretty (A x y) = pretty (x, y)
  pretty O       = "O"

-------------------------------------------------------------------------------
-- Jacobian coordinates
-------------------------------------------------------------------------------

-- | Weierstrass Jacobian points.
type WJPoint = WPoint 'Jacobian

-- | Weierstrass Jacobian curves @y^2 = x^3 + Ax + B@.
class WCurve 'Jacobian e q r => WJCurve e q r where
  {-# MINIMAL gJ_ #-}
  gJ_ :: WJPoint e q r -- ^ Curve generator.

-- Weierstrass Jacobian curves are elliptic curves.
instance WJCurve e q r => Curve 'Weierstrass 'Jacobian e q r where

  data instance Point 'Weierstrass 'Jacobian e q r = J q q q -- ^ Jacobian point.
    deriving (Generic, NFData, Read, Show)

  -- Addition formula add-2007-bl
  add  p           (J  _  _  0) = p
  add (J  _  _  0)  q           = q
  add (J x1 y1 z1) (J x2 y2 z2) = J x3 y3 z3
    where
      z1z1 = z1 * z1
      z2z2 = z2 * z2
      z1z2 = z1 + z2
      u1   = x1 * z2z2
      u2   = x2 * z1z1
      s1   = y1 * z2 * z2z2
      s2   = y2 * z1 * z1z1
      h    = u2 - u1
      h2   = 2 * h
      i    = h2 * h2
      j    = h * i
      r    = 2 * (s2 - s1)
      v    = u1 * i
      x3   = r * r - j - 2 * v
      y3   = r * (v - x3) - 2 * s1 * j
      z3   = (z1z2 * z1z2 - z1z1 - z2z2) * h
  {-# INLINABLE add #-}

  char = q_
  {-# INLINABLE char #-}

  cof = h_
  {-# INLINABLE cof #-}

  -- Doubling formula dbl-2007-bl
  dbl (J  _  _  0) = J  1  1  0
  dbl (J x1 y1 z1) = J x3 y3 z3
    where
      a    = a_ (witness :: WJPoint e q r)
      xx   = x1 * x1
      yy   = y1 * y1
      yyyy = yy * yy
      zz   = z1 * z1
      xy   = x1 + yy
      yz   = y1 + z1
      s    = 2 * (xy * xy - xx - yyyy)
      m    = 3 * xx + a * zz * zz
      t    = m * m - 2 * s
      x3   = t
      y3   = m * (s - t) - 8 * yyyy
      z3   = yz * yz - yy - zz
  {-# INLINABLE dbl #-}

  def (J x y z) = y * y == x * x * x + zz * zz * (a * x + b * zz)
    where
      a  = a_ (witness :: WJPoint e q r)
      b  = b_ (witness :: WJPoint e q r)
      zz = z * z
  {-# INLINABLE def #-}

  disc _ = 4 * a * a * a + 27 * b * b
    where
      a = a_ (witness :: WJPoint e q r)
      b = b_ (witness :: WJPoint e q r)
  {-# INLINABLE disc #-}

  frob (J x y z) = J (F.frob x) (F.frob y) (F.frob z)
  {-# INLINABLE frob #-}

  fromA (A x y) = J x y 1
  fromA _       = J 1 1 0
  {-# INLINABLE fromA #-}

  gen = gJ_
  {-# INLINABLE gen #-}

  id = J 1 1 0
  {-# INLINABLE id #-}

  inv (J x y z) = J x (-y) z
  {-# INLINABLE inv #-}

  order = r_
  {-# INLINABLE order #-}

  point x y = let p = J x y 1 in if def p then Just p else Nothing
  {-# INLINABLE point #-}

  pointX x = flip (J x) 1 <$> yX (witness :: WJPoint e q r) x
  {-# INLINABLE pointX #-}

  toA (J _ _ 0) = O
  toA (J x y z) = let zz = z * z in A (x / zz) (y / (z * zz))
  {-# INLINABLE toA #-}

  yX _ x = sr (((x * x + a) * x) + b)
    where
      a = a_ (witness :: WJPoint e q r)
      b = b_ (witness :: WJPoint e q r)
  {-# INLINABLE yX #-}

-- Weierstrass Jacobian points are equatable.
instance WJCurve e q r => Eq (WJPoint e q r) where

  J x1 y1 z1 == J x2 y2 z2 = z1 == 0 && z2 == 0
    || x1 * zz2 == x2 * zz1 && y1 * z2 * zz2 == y2 * z1 * zz1
    where
      zz1 = z1 * z1
      zz2 = z2 * z2
  {-# INLINABLE (==) #-}

-- Weierstrass Jacobian points are pretty.
instance WJCurve e q r => Pretty (WJPoint e q r) where

  pretty (J x y z) = pretty (x, y, z)

-------------------------------------------------------------------------------
-- Projective coordinates
-------------------------------------------------------------------------------

-- | Weierstrass projective points.
type WPPoint = WPoint 'Projective

-- | Weierstrass projective curves @y^2 = x^3 + Ax + B@.
class WCurve 'Projective e q r => WPCurve e q r where
  {-# MINIMAL gP_ #-}
  gP_ :: WPPoint e q r -- ^ Curve generator.

-- Weierstrass projective curves are elliptic curves.
instance WPCurve e q r => Curve 'Weierstrass 'Projective e q r where

  data instance Point 'Weierstrass 'Projective e q r = P q q q -- ^ Projective point.
    deriving (Generic, NFData, Read, Show)

  -- Addition formula add-1998-cmo-2
  add  p           (P  _  _  0) = p
  add (P  _  _  0)  q           = q
  add (P x1 y1 z1) (P x2 y2 z2) = P x3 y3 z3
    where
      y1z2 = y1 * z2
      x1z2 = x1 * z2
      z1z2 = z1 * z2
      u    = y2 * z1 - y1z2
      uu   = u * u
      v    = x2 * z1 - x1z2
      vv   = v * v
      vvv  = v * vv
      r    = vv * x1z2
      a    = uu * z1z2 - vvv - 2 * r
      x3   = v * a
      y3   = u * (r - a) - vvv * y1z2
      z3   = vvv * z1z2
  {-# INLINABLE add #-}

  char = q_
  {-# INLINABLE char #-}

  cof = h_
  {-# INLINABLE cof #-}

  -- Doubling formula dbl-2007-bl
  dbl (P  _  _  0) = P  0  1  0
  dbl (P x1 y1 z1) = P x3 y3 z3
    where
      a   = a_ (witness :: WPPoint e q r)
      xx  = x1 * x1
      zz  = z1 * z1
      w   = a * zz + 3 * xx
      s   = 2 * y1 * z1
      ss  = s * s
      sss = s * ss
      r   = y1 * s
      rr  = r * r
      xr  = x1 + r
      b   = xr * xr - xx - rr
      h   = w * w - 2 * b
      x3  = h * s
      y3  = w * (b - h) - 2 * rr
      z3  = sss
  {-# INLINABLE dbl #-}

  def (P x y z) = (x * x + a * zz) * x == (y * y - b * zz) * z
    where
      a  = a_ (witness :: WPPoint e q r)
      b  = b_ (witness :: WPPoint e q r)
      zz = z * z
  {-# INLINABLE def #-}

  disc _ = 4 * a * a * a + 27 * b * b
    where
      a = a_ (witness :: WPPoint e q r)
      b = b_ (witness :: WPPoint e q r)
  {-# INLINABLE disc #-}

  frob (P x y z) = P (F.frob x) (F.frob y) (F.frob z)
  {-# INLINABLE frob #-}

  fromA (A x y) = P x y 1
  fromA _       = P 0 1 0
  {-# INLINABLE fromA #-}

  gen = gP_
  {-# INLINABLE gen #-}

  id = P 0 1 0
  {-# INLINABLE id #-}

  inv (P x y z) = P x (-y) z
  {-# INLINABLE inv #-}

  order = r_
  {-# INLINABLE order #-}

  point x y = let p = P x y 1 in if def p then Just p else Nothing
  {-# INLINABLE point #-}

  pointX x = flip (P x) 1 <$> yX (witness :: WPPoint e q r) x
  {-# INLINABLE pointX #-}

  toA (P _ _ 0) = O
  toA (P x y z) = A (x / z) (y / z)
  {-# INLINABLE toA #-}

  yX _ x = sr (((x * x + a) * x) + b)
    where
      a = a_ (witness :: WPPoint e q r)
      b = b_ (witness :: WPPoint e q r)
  {-# INLINABLE yX #-}

-- Weierstrass projective points are equatable.
instance WPCurve e q r => Eq (WPPoint e q r) where

  P x1 y1 z1 == P x2 y2 z2 = z1 == 0 && z2 == 0
    || x1 * z2 == x2 * z1 && y1 * z2 == y2 * z1
  {-# INLINABLE (==) #-}

-- Weierstrass projective points are pretty.
instance WPCurve e q r => Pretty (WPPoint e q r) where

  pretty (P x y z) = pretty (x, y, z)
