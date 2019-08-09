module Generate.Types
  ( module Generate.Types
  ) where

import Protolude

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Element = BF Integer
             | EF [Element]
             | PF Integer

data Field = BinaryField Text Integer
           | ExtensionField Text Text Text Text (Maybe Field)
           | PrimeField Text Integer

data Types = Types
  { curve   :: Text
  , field   :: Field
  , field'  :: Field
  , imports :: Maybe Text
  }
