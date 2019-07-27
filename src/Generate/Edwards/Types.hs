module Generate.Edwards.Types
  ( Curve(..)
  , Element(..)
  , Field(..)
  , Parameters(..)
  , Types(..)
  ) where

import Protolude

import Generate.Types

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Curve = Curve
  { types      :: Types
  , parameters :: Parameters
  }

data Parameters = Parameters
  { a :: Element
  , d :: Element
  , x :: Element
  , y :: Element
  , h :: Integer
  , n :: Integer
  , p :: Integer
  }

data Types = Types
  { curve   :: Text
  , field   :: Field
  , imports :: Maybe Text
  }
