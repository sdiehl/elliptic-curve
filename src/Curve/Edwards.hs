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
  , Point(..)
  , fromAtoP
  , fromPtoA
  ) where

import Protolude

import Control.Monad.Random (Random(..))
import GaloisField (GaloisField(..))
import PrimeField (PrimeField)
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
class (GaloisField q, GaloisField r, Curve 'Edwards c e q r)
  => ECurve c e q r where
  {-# MINIMAL a_, d_, h_, q_, r_, x_, y_ #-}
  a_ :: EPoint c e q r -> q       -- ^ Coefficient @A@.
  d_ :: EPoint c e q r -> q       -- ^ Coefficient @D@.
  h_ :: EPoint c e q r -> Integer -- ^ Curve cofactor.
  q_ :: EPoint c e q r -> Integer -- ^ Curve characteristic.
  r_ :: EPoint c e q r -> Integer -- ^ Curve order.
  x_ :: EPoint c e q r -> q       -- ^ Coordinate @X@.
  y_ :: EPoint c e q r -> q       -- ^ Coordinate @Y@.

-- | Edwards coordinates.
data Coordinates = Affine
                 | Projective

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Edwards affine curves.
type EAPoint = EPoint 'Affine

-- | Edwards affine curves @Ax^2 + y^2 = 1 + Dx^2y^2@.
class ECurve 'Affine e q r => EACurve e q r where
  {-# MINIMAL gA_ #-}
  gA_ :: EAPoint e q r -- ^ Curve generator.

-- Edwards affine curves are elliptic curves.
instance (KnownNat p, EACurve e q (PrimeField p))
  => Curve 'Edwards 'Affine e q (PrimeField p) where

  data instance Point 'Edwards 'Affine e q (PrimeField p)
    = A q q -- ^ Affine point.
    deriving (Eq, Generic, NFData, Read, Show)

  char = q_
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = d * (1 - d)
    where
      d = d_ (witness :: EAPoint e q (PrimeField p))
  {-# INLINE disc #-}

  point x y = let p = A x y in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = A x <$> yX (witness :: EAPoint e q (PrimeField p)) x
  {-# INLINE pointX #-}

  yX _ x = sr ((1 - a * xx) / (1 - d * xx))
    where
      a  = a_ (witness :: EAPoint e q (PrimeField p))
      d  = d_ (witness :: EAPoint e q (PrimeField p))
      xx = x * x
  {-# INLINE yX #-}

-- Edwards affine points are groups.
instance (KnownNat p, EACurve e q (PrimeField p))
  => Group (EAPoint e q (PrimeField p)) where

  add (A x1 y1) (A x2 y2) = A x3 y3
    where
      a    = a_ (witness :: EAPoint e q (PrimeField p))
      d    = d_ (witness :: EAPoint e q (PrimeField p))
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

  def (A x y) = a * xx + yy == 1 + d * xx * yy
    where
      a  = a_ (witness :: EAPoint e q (PrimeField p))
      d  = d_ (witness :: EAPoint e q (PrimeField p))
      xx = x * x
      yy = y * y
  {-# INLINE def #-}

  gen = gA_
  {-# INLINE gen #-}

  id = A 0 1
  {-# INLINE id #-}

  inv (A x y) = A (-x) y
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Edwards affine points are arbitrary.
instance (KnownNat p, EACurve e q (PrimeField p))
  => Arbitrary (EAPoint e q (PrimeField p)) where
  arbitrary = suchThatMap arbitrary pointX

-- Edwards affine points are pretty.
instance (KnownNat p, EACurve e q (PrimeField p))
  => Pretty (EAPoint e q (PrimeField p)) where
  pretty (A x y) = pretty (x, y)

-- Edwards affine points are random.
instance (KnownNat p, EACurve e q (PrimeField p)) 
  => Random (EAPoint e q (PrimeField p)) where
  random g = let (x, g') = random g in case pointX x of
    Just p -> (p, g')
    _      -> random g'
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Projective coordinates
-------------------------------------------------------------------------------

-- | Edwards projective curves.
type EPPoint = EPoint 'Projective

-- | Edwards projective curves @Ax^2z^2 + y^2z^2 = z^4 + Dx^2y^2@.
class ECurve 'Projective e q r => EPCurve e q r where
  {-# MINIMAL gP_ #-}
  gP_ :: EPPoint e q r -- ^ Curve generator.

-- Edwards projective curves are elliptic curves.
instance (KnownNat p, EPCurve e q (PrimeField p))
  => Curve 'Edwards 'Projective e q (PrimeField p) where

  data instance Point 'Edwards 'Projective e q (PrimeField p)
    = P q q q -- ^ Projective point.
    deriving (Generic, NFData, Read, Show)

  char = q_
  {-# INLINE char #-}

  cof = h_
  {-# INLINE cof #-}

  disc _ = d * (1 - d)
    where
      d = d_ (witness :: EPPoint e q (PrimeField p))
  {-# INLINE disc #-}

  point x y = let p = P x y 1 in if def p then Just p else Nothing
  {-# INLINE point #-}

  pointX x = flip (P x) 1 <$> yX (witness :: EPPoint e q (PrimeField p)) x
  {-# INLINE pointX #-}

  yX _ x = sr ((1 - a * xx) / (1 - d * xx))
    where
      a  = a_ (witness :: EPPoint e q (PrimeField p))
      d  = d_ (witness :: EPPoint e q (PrimeField p))
      xx = x * x
  {-# INLINE yX #-}

-- Edwards projective points are groups.
instance (KnownNat p, EPCurve e q (PrimeField p))
  => Group (EPPoint e q (PrimeField p)) where

  -- Addition formula add-2008-bbjlp
  add (P x1 y1 z1) (P x2 y2 z2) = P x3 y3 z3
    where
      a  = z1 * z2
      b  = a * a
      c  = x1 * x2
      d  = y1 * y2
      e  = d_ (witness :: EPPoint e q (PrimeField p)) * c * d
      f  = b - e
      g  = b + e
      x3 = a * f * ((x1 + y1) * (x2 + y2) - c - d)
      y3 = a * g * (d - a_ (witness :: EPPoint e q (PrimeField p)) * c)
      z3 = f * g
  {-# INLINE add #-}

  -- Doubling formula dbl-2008-bbjlp
  dbl (P x1 y1 z1) = P x3 y3 z3
    where
      a  = a_ (witness :: EPPoint e q (PrimeField p))
      xy = x1 + y1
      b  = xy * xy
      c  = x1 * x1
      d  = y1 * y1
      e  = a * c
      f  = e + d
      h  = z1 * z1
      j  = f - 2 * h
      x3 = (b - c - d) * j
      y3 = f * (e - d)
      z3 = f * j
  {-# INLINE dbl #-}

  def (P x y z) = (a * xx + yy - zz) * zz == d * xx * yy
    where
      a  = a_ (witness :: EPPoint e q (PrimeField p))
      d  = d_ (witness :: EPPoint e q (PrimeField p))
      xx = x * x
      yy = y * y
      zz = z * z
  {-# INLINE def #-}

  gen = gP_
  {-# INLINE gen #-}

  id = P 0 1 1
  {-# INLINE id #-}

  inv (P x y z) = P (-x) y z
  {-# INLINE inv #-}

  order = r_
  {-# INLINE order #-}

-- Edwards projective points are arbitrary.
instance (KnownNat p, EPCurve e q (PrimeField p))
  => Arbitrary (EPPoint e q (PrimeField p)) where
  arbitrary = suchThatMap arbitrary pointX

-- Edwards projective points are equatable.
instance (KnownNat p, EPCurve e q (PrimeField p))
  => Eq (EPPoint e q (PrimeField p)) where
  P x1 y1 z1 == P x2 y2 z2 = z1 == 0 && z2 == 0
    || x1 * z2 == x2 * z1 && y1 * z2 == y2 * z1

-- Edwards projective points are pretty.
instance (KnownNat p, EPCurve e q (PrimeField p))
  => Pretty (EPPoint e q (PrimeField p)) where
  pretty (P x y z) = pretty (x, y, z)

-- Edwards projective points are random.
instance (KnownNat p, EPCurve e q (PrimeField p))
  => Random (EPPoint e q (PrimeField p)) where
  random g = let (x, g') = random g in case pointX x of
    Just p -> (p, g')
    _      -> random g'
  {-# INLINE random #-}
  randomR = panic "not implemented."

-------------------------------------------------------------------------------
-- Coordinate transformations
-------------------------------------------------------------------------------

-- | Transform from affine coordinates to projective coordinates.
fromAtoP :: (KnownNat p, EACurve e q (PrimeField p), EPCurve e q (PrimeField p))
  => EAPoint e q (PrimeField p) -> EPPoint e q (PrimeField p)
fromAtoP (A x y) = P x y 1
{-# INLINE fromAtoP #-}

-- | Transform from projective coordinates to affine coordinates.
fromPtoA :: (KnownNat p, EACurve e q (PrimeField p), EPCurve e q (PrimeField p))
  => EPPoint e q (PrimeField p) -> EAPoint e q (PrimeField p)
fromPtoA (P x y z) = A (x / z) (y / z)
{-# INLINE fromPtoA #-}
