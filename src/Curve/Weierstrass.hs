module Curve.Weierstrass
  ( Coordinates(..)
  , Curve(..)
  , Form(..)
  , Group(..)
  , WCurve(..)
  , WPoint
  , WACurve(..)
  , WAPoint
  , WJPoint
  , WJCurve(..)
  , WPPoint
  , WPCurve(..)
  , pattern A
  , pattern J
  , pattern P
  ) where

import Protolude

import Control.Monad.Random (Random(..), RandomGen)
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..), Gen)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Curve(..), Form(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Weierstrass form
-------------------------------------------------------------------------------

-- | Weierstrass points.
type WPoint = Point 'Weierstrass

-- | Weierstrass curves.
class (GaloisField k, Curve 'Weierstrass c e k) => WCurve c e k where
  {-# MINIMAL a_, b_, h_, q_, r_, x_, y_ #-}
  a_ :: WPoint c e k -> k       -- ^ Coefficient @A@.
  b_ :: WPoint c e k -> k       -- ^ Coefficient @B@.
  h_ :: WPoint c e k -> Integer -- ^ Curve cofactor.
  q_ :: WPoint c e k -> Integer -- ^ Curve characteristic.
  r_ :: WPoint c e k -> Integer -- ^ Curve order.
  x_ :: WPoint c e k -> k       -- ^ Coordinate @X@.
  y_ :: WPoint c e k -> k       -- ^ Coordinate @Y@.

-- | Weierstrass coordinates.
data Coordinates = Affine
                 | Jacobian
                 | Projective

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Weierstrass affine points.
type WAPoint = WPoint 'Affine

-- | Weierstrass affine curves @y^2 = x^3 + Ax + B@.
class WCurve 'Affine e k => WACurve e k where
  {-# MINIMAL gA_ #-}
  gA_ :: WAPoint e k -- ^ Curve generator.

-- | Weierstrass affine points patterns.
pattern A :: WACurve e k => k -> k -> WAPoint e k
pattern A x y <- A' x y

-- Weierstrass affine curves are elliptic curves.
instance WACurve e k => Curve 'Weierstrass 'Affine e k where

  data instance Point 'Weierstrass 'Affine e k = A' k k -- ^ Affine point.
                                               | O      -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)

  char = q_
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = 4 * pow a 3 + 27 * pow b 2
    where
      a = a_ (witness :: WAPoint e k)
      b = b_ (witness :: WAPoint e k)
  {-# INLINE disc #-}

  point x y = let p = A' x y in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = A' x <$> yX (witness :: WAPoint e k) x
  {-# INLINE pointX #-}

  yX _ x = sr (((pow x 2 + a) * x) + b)
    where
      a = a_ (witness :: WAPoint e k)
      b = b_ (witness :: WAPoint e k)
  {-# INLINE yX #-}

-- Weierstrass affine points are groups.
instance WACurve e k => Group (WAPoint e k) where

  add p O       = p
  add O q       = q
  add (A' x1 y1) (A' x2 y2)
    | x1 == x2  = O
    | otherwise = A' x3 y3
    where
      l  = (y1 - y2) / (x1 - x2)
      x3 = pow l 2 - x1 - x2
      y3 = l * (x1 - x3) - y1
  {-# INLINE add #-}

  dbl O         = O
  dbl (A' x y)
    | y == 0    = O
    | otherwise = A' x' y'
    where
      l  = (3 * pow x 2 + a_ (witness :: WAPoint e k)) / (2 * y)
      x' = pow l 2 - 2 * x
      y' = l * (x - x') - y
  {-# INLINE dbl #-}

  def O        = True
  def (A' x y) = pow y 2 == (pow x 2 + a) * x + b
    where
      a = a_ (witness :: WAPoint e k)
      b = b_ (witness :: WAPoint e k)
  {-# INLINE def #-}

  gen = gA_
  {-# INLINE gen #-}

  id = O
  {-# INLINE id #-}

  inv O        = O
  inv (A' x y) = A' x (-y)
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Weierstrass affine points are arbitrary.
instance WACurve e k => Arbitrary (WAPoint e k) where
  arbitrary = mul' gA_ <$> (arbitrary :: Gen Integer) -- TODO
  -- arbitrary = suchThatMap arbitrary pointX

-- Weierstrass affine points are pretty.
instance WACurve e k => Pretty (WAPoint e k) where
  pretty (A' x y) = pretty (x, y)
  pretty O        = "O"

-- Weierstrass affine points are random.
instance WACurve e k => Random (WAPoint e k) where
  random  = first (mul' gA_) . (random :: RandomGen g => g -> (Integer, g)) -- TODO
  -- random g = case pointX x of
  --   Just p -> (p, g')
  --   _      -> random g'
  --   where
  --     (x, g') = random g
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Jacobian coordinates
-------------------------------------------------------------------------------

-- | Weierstrass Jacobian points.
type WJPoint = WPoint 'Jacobian

-- | Weierstrass Jacobian curves @y^2 = x^3 + Ax + B@.
class WCurve 'Jacobian e k => WJCurve e k where
  {-# MINIMAL gJ_ #-}
  gJ_ :: WJPoint e k -- ^ Curve generator.

-- | Weierstrass Jacobian points patterns.
pattern J :: WJCurve e k => k -> k -> k -> WJPoint e k
pattern J x y z <- J' x y z

-- Weierstrass Jacobian curves are elliptic curves.
instance WJCurve e k => Curve 'Weierstrass 'Jacobian e k where

  data instance Point 'Weierstrass 'Jacobian e k = J' k k k -- ^ Jacobian point.
    deriving (Generic, NFData, Read, Show)

  char = q_
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = 4 * pow a 3 + 27 * pow b 2
    where
      a = a_ (witness :: WJPoint e k)
      b = b_ (witness :: WJPoint e k)
  {-# INLINE disc #-}

  point x y = let p = J' x y 1 in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = flip (J' x) 1 <$> yX (witness :: WJPoint e k) x
  {-# INLINE pointX #-}

  yX _ x = sr (((pow x 2 + a) * x) + b)
    where
      a = a_ (witness :: WJPoint e k)
      b = b_ (witness :: WJPoint e k)
  {-# INLINE yX #-}

-- Weierstrass Jacobian points are groups.
instance WJCurve e k => Group (WJPoint e k) where

  -- | Addition formula add-2007-bl
  add  p            (J'  _  _  0) = p
  add (J'  _  _  0)  q            = q
  add (J' x1 y1 z1) (J' x2 y2 z2) = J' x3 y3 z3
    where
      z1z1 = pow z1 2
      z2z2 = pow z2 2
      u1   = x1 * z2z2
      u2   = x2 * z1z1
      s1   = y1 * z2 * z2z2
      s2   = y2 * z1 * z1z1
      h    = u2 - u1
      i    = pow (2 * h) 2
      j    = h * i
      r    = 2 * (s2 - s1)
      v    = u1 * i
      x3   = pow r 2 - j - 2 * v
      y3   = r * (v - x3) - 2 * s1 * j
      z3   = (pow (z1 + z2) 2 - z1z1 - z2z2) * h
  {-# INLINE add #-}

  -- | Doubling formula dbl-2007-bl
  dbl (J'  _  _  0) = J'  1  1  0
  dbl (J' x1 y1 z1) = J' x3 y3 z3
    where
      xx   = pow x1 2
      yy   = pow y1 2
      yyyy = pow yy 2
      zz   = pow z1 2
      s    = 2 * (pow (x1 + yy) 2 - xx - yyyy)
      m    = 3 * xx + a_ (witness :: WJPoint e k) * pow zz 2
      t    = pow m 2 - 2 * s
      x3   = t
      y3   = m * (s - t) - 8 * yyyy
      z3   = pow (y1 + z1) 2 - yy - zz
  {-# INLINE dbl #-}

  def (J' x y z) = pow y 2 == pow x 3 + pow zz 2 * (a * x + b * zz)
    where
      a  = a_ (witness :: WJPoint e k)
      b  = b_ (witness :: WJPoint e k)
      zz = pow z 2
  {-# INLINE def #-}

  gen = gJ_
  {-# INLINE gen #-}

  id = J' 1 1 0
  {-# INLINE id #-}

  inv (J' x y z) = J' x (-y) z
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Weierstrass Jacobian points are arbitrary.
instance WJCurve e k => Arbitrary (WJPoint e k) where
  arbitrary = mul' gJ_ <$> (arbitrary :: Gen Integer) -- TODO
  -- arbitrary = suchThatMap arbitrary pointX

-- Weierstrass Jacobian points are equatable.
instance WJCurve e k => Eq (WJPoint e k) where
  J' x1 y1 z1 == J' x2 y2 z2 = z1 == 0 && z2 == 0
    || x1 * pow z2 2 == x2 * pow z1 2 && y1 * pow z2 3 == y2 * pow z1 3

-- Weierstrass Jacobian points are pretty.
instance WJCurve e k => Pretty (WJPoint e k) where
  pretty (J' x y z) = pretty (x, y, z)

-- Weierstrass Jacobian points are random.
instance WJCurve e k => Random (WJPoint e k) where
  random  = first (mul' gJ_) . (random :: RandomGen g => g -> (Integer, g)) -- TODO
  -- random g = case pointX x of
  --   Just p -> (p, g')
  --   _      -> random g'
  --   where
  --     (x, g') = random g
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Projective coordinates
-------------------------------------------------------------------------------

-- | Weierstrass projective points.
type WPPoint = WPoint 'Projective

-- | Weierstrass projective curves @y^2 = x^3 + Ax + B@.
class WCurve 'Projective e k => WPCurve e k where
  {-# MINIMAL gP_ #-}
  gP_ :: WPPoint e k -- ^ Curve generator.

-- | Weierstrass projective points patterns.
pattern P :: WPCurve e k => k -> k -> k -> WPPoint e k
pattern P x y z <- P' x y z

-- Weierstrass projective curves are elliptic curves.
instance WPCurve e k => Curve 'Weierstrass 'Projective e k where

  data instance Point 'Weierstrass 'Projective e k = P' k k k -- ^ Projective point.
    deriving (Generic, NFData, Read, Show)

  char = q_
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = 4 * pow a 3 + 27 * pow b 2
    where
      a = a_ (witness :: WPPoint e k)
      b = b_ (witness :: WPPoint e k)
  {-# INLINE disc #-}

  point x y = let p = P' x y 1 in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = flip (P' x) 1 <$> yX (witness :: WPPoint e k) x
  {-# INLINE pointX #-}

  yX _ x = sr (((pow x 2 + a) * x) + b)
    where
      a = a_ (witness :: WPPoint e k)
      b = b_ (witness :: WPPoint e k)
  {-# INLINE yX #-}

-- Weierstrass projective points are groups.
instance WPCurve e k => Group (WPPoint e k) where

  -- | Addition formula add-1998-cmo-2
  add  p            (P'  _  _  0) = p
  add (P'  _  _  0)  q            = q
  add (P' x1 y1 z1) (P' x2 y2 z2) = P' x3 y3 z3
    where
      y1z2 = y1 * z2
      x1z2 = x1 * z2
      z1z2 = z1 * z2
      u    = y2 * z1 - y1z2
      uu   = pow u 2
      v    = x2 * z1 - x1z2
      vv   = pow v 2
      vvv  = v * vv
      r    = vv * x1z2
      a    = uu * z1z2 - vvv - 2 * r
      x3   = v * a
      y3   = u * (r - a) - vvv * y1z2
      z3   = vvv * z1z2
  {-# INLINE add #-}

  -- | Doubling formula dbl-2007-bl
  dbl (P'  _  _  0) = P'  0  1  0
  dbl (P' x1 y1 z1) = P' x3 y3 z3
    where
      xx  = pow x1 2
      zz  = pow z1 2
      w   = a_ (witness :: WPPoint e k) * zz + 3 * xx
      s   = 2 * y1 * z1
      ss  = pow s 2
      sss = s * ss
      r   = y1 * s
      rr  = pow r 2
      b   = pow (x1 + r) 2 - xx - rr
      h   = pow w 2 - 2 * b
      x3  = h * s
      y3  = w * (b - h) - 2 * rr
      z3  = sss
  {-# INLINE dbl #-}

  def (P' x y z) = (pow x 2 + a * zz) * x == (pow y 2 - b * zz) * z
    where
      a  = a_ (witness :: WPPoint e k)
      b  = b_ (witness :: WPPoint e k)
      zz = pow z 2
  {-# INLINE def #-}

  gen = gP_
  {-# INLINE gen #-}

  id = P' 0 1 0
  {-# INLINE id #-}

  inv (P' x y z) = P' x (-y) z
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Weierstrass projective points are arbitrary.
instance WPCurve e k => Arbitrary (WPPoint e k) where
  arbitrary = mul' gP_ <$> (arbitrary :: Gen Integer) -- TODO
  -- arbitrary = suchThatMap arbitrary pointX

-- Weierstrass projective points are equatable.
instance WPCurve e k => Eq (WPPoint e k) where
  P' x1 y1 z1 == P' x2 y2 z2 = z1 == 0 && z2 == 0
    || x1 * z2 == x2 * z1 && y1 * z2 == y2 * z1

-- Weierstrass projective points are pretty.
instance WPCurve e k => Pretty (WPPoint e k) where
  pretty (P' x y z) = pretty (x, y, z)

-- Weierstrass projective points are random.
instance WPCurve e k => Random (WPPoint e k) where
  random  = first (mul' gP_) . (random :: RandomGen g => g -> (Integer, g)) -- TODO
  -- random g = case pointX x of
  --   Just p -> (p, g')
  --   _      -> random g'
  --   where
  --     (x, g') = random g
  {-# INLINE random #-}
  randomR = panic "not implemented."
