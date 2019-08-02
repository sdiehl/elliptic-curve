module Curve.Edwards
  ( Coordinates(..)
  , Curve(..)
  , ECurve(..)
  , EPoint
  , EACurve(..)
  , EAPoint
  , EPCurve(..)
  , EPPoint
  , Form(..)
  , Group(..)
  , pattern A
  , pattern P
  ) where

import Protolude

import Control.Monad.Random (Random(..))
import GaloisField (GaloisField(..))
import Test.Tasty.QuickCheck (Arbitrary(..), suchThatMap)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Curve (Curve(..), Form(..))
import Group (Group(..))

-------------------------------------------------------------------------------
-- Edwards form
-------------------------------------------------------------------------------

-- | Edwards points.
type EPoint = Point 'Edwards

-- | Edwards curves.
class (GaloisField k, Curve 'Edwards c e k) => ECurve c e k where
  {-# MINIMAL a_, d_, h_, q_, r_, x_, y_ #-}
  a_ :: EPoint c e k -> k       -- ^ Coefficient @A@.
  d_ :: EPoint c e k -> k       -- ^ Coefficient @D@.
  h_ :: EPoint c e k -> Integer -- ^ Curve cofactor.
  q_ :: EPoint c e k -> Integer -- ^ Curve characteristic.
  r_ :: EPoint c e k -> Integer -- ^ Curve order.
  x_ :: EPoint c e k -> k       -- ^ Coordinate @X@.
  y_ :: EPoint c e k -> k       -- ^ Coordinate @Y@.

-- | Edwards coordinates.
data Coordinates = Affine
                 | Projective

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Edwards affine curves.
type EAPoint = EPoint 'Affine

-- | Edwards affine curves @Ax^2 + y^2 = 1 + Dx^2y^2@.
class ECurve 'Affine e k => EACurve e k where
  {-# MINIMAL gA_ #-}
  gA_ :: EAPoint e k -- ^ Curve generator.

-- | Edwards affine points patterns.
pattern A :: EACurve e k => k -> k -> EAPoint e k
pattern A x y <- A' x y

-- Edwards affine curves are elliptic curves.
instance EACurve e k => Curve 'Edwards 'Affine e k where

  data instance Point 'Edwards 'Affine e k = A' k k -- ^ Affine point.
    deriving (Eq, Generic, NFData, Read, Show)

  char = q_
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = d * (1 - d)
    where
      d = d_ (witness :: EAPoint e k)
  {-# INLINE disc #-}

  point x y = let p = A' x y in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = A' x <$> yX (witness :: EAPoint e k) x
  {-# INLINE pointX #-}

  yX _ x = sr ((1 - a * xx) / (1 - d * xx))
    where
      a  = a_ (witness :: EAPoint e k)
      d  = d_ (witness :: EAPoint e k)
      xx = pow x 2
  {-# INLINE yX #-}

-- Edwards affine points are groups.
instance EACurve e k => Group (EAPoint e k) where

  add (A' x1 y1) (A' x2 y2) = A' x3 y3
    where
      a    = a_ (witness :: EAPoint e k)
      d    = d_ (witness :: EAPoint e k)
      x1x2 = x1 * x2
      y1y2 = y1 * y2
      x1y2 = x1 * y2
      x2y1 = x2 * y1
      dxy  = d * x1x2 * y1y2
      x3   = (x1y2 + x2y1) / (1 + dxy)
      y3   = (y1y2 - a * x1x2) / (1 - dxy)
  {-# INLINE add #-}

  dbl = join add
  {-# INLINE dbl #-}

  def (A' x y) = a * xx + yy == 1 + d * xx * yy
    where
      a  = a_ (witness :: EAPoint e k)
      d  = d_ (witness :: EAPoint e k)
      xx = pow x 2
      yy = pow y 2
  {-# INLINE def #-}

  gen = gA_
  {-# INLINE gen #-}

  id = A' 0 1
  {-# INLINE id #-}

  inv (A' x y) = A' (-x) y
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Edwards affine points are arbitrary.
instance EACurve e k => Arbitrary (EAPoint e k) where
  arbitrary = suchThatMap arbitrary pointX

-- Edwards affine points are pretty.
instance EACurve e k => Pretty (EAPoint e k) where
  pretty (A' x y) = pretty (x, y)

-- Edwards affine points are random.
instance EACurve e k => Random (EAPoint e k) where
  random g = case pointX x of
    Just p -> (p, g')
    _      -> random g'
    where
      (x, g') = random g
  {-# INLINE random #-}
  randomR  = panic "not implemented."

-------------------------------------------------------------------------------
-- Projective coordinates
-------------------------------------------------------------------------------

-- | Edwards projective curves.
type EPPoint = EPoint 'Projective

-- | Edwards projective curves @Ax^2z^2 + y^2z^2 = z^4 + Dx^2y^2@.
class ECurve 'Projective e k => EPCurve e k where
  {-# MINIMAL gP_ #-}
  gP_ :: EPPoint e k -- ^ Curve generator.

-- | Edwards projective points patterns.
pattern P :: EPCurve e k => k -> k -> k -> EPPoint e k
pattern P x y z <- P' x y z

-- Edwards projective curves are elliptic curves.
instance EPCurve e k => Curve 'Edwards 'Projective e k where

  data instance Point 'Edwards 'Projective e k = P' k k k -- ^ Projective point.
    deriving (Generic, NFData, Read, Show)

  char = q_
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = d * (1 - d)
    where
      d = d_ (witness :: EPPoint e k)
  {-# INLINE disc #-}

  point x y = let p = P' x y 1 in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = flip (P' x) 1 <$> yX (witness :: EPPoint e k) x
  {-# INLINE pointX #-}

  yX _ x = sr ((1 - a * xx) / (1 - d * xx))
    where
      a  = a_ (witness :: EPPoint e k)
      d  = d_ (witness :: EPPoint e k)
      xx = pow x 2
  {-# INLINE yX #-}

-- Edwards projective points are groups.
instance EPCurve e k => Group (EPPoint e k) where

  -- | Addition formula add-2008-bbjlp
  add (P' x1 y1 z1) (P' x2 y2 z2) = P' x3 y3 z3
    where
      a  = z1 * z2
      b  = pow a 2
      c  = x1 * x2
      d  = y1 * y2
      e  = d_ (witness :: EPPoint e k) * c * d
      f  = b - e
      g  = b + e
      x3 = a * f * ((x1 + y1) * (x2 + y2) - c - d)
      y3 = a * g * (d - a_ (witness :: EPPoint e k) * c)
      z3 = f * g
  {-# INLINE add #-}

  -- | Doubling formula dbl-2008-bbjlp
  dbl (P' x1 y1 z1) = P' x3 y3 z3
    where
      b  = pow (x1 + y1) 2
      c  = pow x1 2
      d  = pow y1 2
      e  = a_ (witness :: EPPoint e k) * c
      f  = e + d
      h  = pow z1 2
      j  = f - 2 * h
      x3 = (b - c - d) * j
      y3 = f * (e - d)
      z3 = f * j
  {-# INLINE dbl #-}

  def (P' x y z) = a * xx * zz + yy * zz == pow zz 2 + d * xx * yy
    where
      a  = a_ (witness :: EPPoint e k)
      d  = d_ (witness :: EPPoint e k)
      xx = pow x 2
      yy = pow y 2
      zz = pow z 2
  {-# INLINE def #-}

  gen = gP_
  {-# INLINE gen #-}

  id = P' 0 1 1
  {-# INLINE id #-}

  inv (P' x y z) = P' (-x) y z
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Edwards projective points are arbitrary.
instance EPCurve e k => Arbitrary (EPPoint e k) where
  arbitrary = suchThatMap arbitrary pointX

-- Edwards projective points are equatable.
instance EPCurve e k => Eq (EPPoint e k) where
  P' x1 y1 z1 == P' x2 y2 z2 = z1 == 0 && z2 == 0
    || x1 * z2 == x2 * z1 && y1 * z2 == y2 * z1

-- Edwards projective points are pretty.
instance EPCurve e k => Pretty (EPPoint e k) where
  pretty (P' x y z) = pretty (x, y, z)

-- Edwards projective points are random.
instance EPCurve e k => Random (EPPoint e k) where
  random g = case pointX x of
    Just p -> (p, g')
    _      -> random g'
    where
      (x, g') = random g
  {-# INLINE random #-}
  randomR  = panic "not implemented."
