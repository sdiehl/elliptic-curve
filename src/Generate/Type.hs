module Generate.Type
  ( Curve(..)
  , Element(..)
  ) where

import Protolude

-------------------------------------------------------------------------------
-- Elements
-------------------------------------------------------------------------------

data Element = BF Integer
             | EF [Element]
             | PF Integer

-------------------------------------------------------------------------------
-- Curves
-------------------------------------------------------------------------------

data Curve = Curve
  { i :: Text
  , a :: Element
  , b :: Element
  , x :: Element
  , y :: Element
  , h :: Integer
  , n :: Integer
  , p :: Integer
  }
