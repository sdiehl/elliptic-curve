module Data.Curve.Edwards
  ( module Data.Curve
  , Point(..)
  -- * Edwards curves
  , ECurve
  , EPoint
  -- ** Edwards affine curves
  , EACurve
  , EAPoint
  -- ** Edwards projective curves
  , EPCurve
  , EPPoint
  ) where

import Data.Curve
import Data.Curve.Edwards.Base
