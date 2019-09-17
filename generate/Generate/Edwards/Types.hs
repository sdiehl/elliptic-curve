module Generate.Edwards.Types
  ( module Generate.Edwards.Types
  , module Generate.Types
  ) where

import Protolude

import GHC.Natural (Natural)

import Generate.Types

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Curve = Curve
  { name       :: Text
  , types      :: Types
  , parameters :: Parameters
  }

data Parameters = Parameters
  { a :: Element
  , d :: Element
  , h :: Natural
  , q :: Natural
  , r :: Natural
  , x :: Element
  , y :: Element
  }
