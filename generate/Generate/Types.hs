module Generate.Types
  ( module Generate.Types
  ) where

import Protolude

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Element = B Integer
             | E [Element]
             | P Integer

data Field = Binary Text Integer
           | Extension Text Text Text Text (Maybe Field)
           | Prime Text Integer

data Types = Types
  { curve   :: Text
  , field   :: Field
  , field'  :: Field
  , imports :: Maybe Text
  }
