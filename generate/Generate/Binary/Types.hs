module Generate.Binary.Types
  ( module Generate.Binary.Types
  , module Generate.Types
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
  , b :: Element
  , h :: Integer
  , p :: Integer
  , r :: Integer
  , x :: Element
  , y :: Element
  }
