module Generate.Weierstrass.Types
  ( Curve(..)
  , Element(..)
  , Field(..)
  , Parameters(..)
  , Types(..)
  ) where

import Protolude

import Generate.Types (Element(..), Field(..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Curve = Curve
  { types      :: Types
  , parameters :: Parameters
  }

data Parameters = Parameters
  { a :: Element
  , b :: Element
  , x :: Element
  , y :: Element
  , h :: Integer
  , n :: Integer
  , p :: Integer
  }

data Types = Types
  { curve :: Text
  , field :: Field
  }
