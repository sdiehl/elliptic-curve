module Generate.Curve
  ( prettyElement
  , prettyField
  , prettyImport
  , prettyType
  ) where

import Protolude

import Text.PrettyPrint.Leijen.Text

import Generate.Pretty (prettyInteger)
import Generate.Types (Element(..), Field(..))

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

prettyElement :: Element -> Doc
prettyElement (BF n)
  = prettyInteger n
prettyElement (EF ns)
  = "fromList " <> align
    (    (if null ns then "[" else "[ ")
    <>   hcat (punctuate "\n, " (map prettyElement ns))
    <$$> "]"
    )
prettyElement (PF n)
  = prettyInteger n

prettyField :: Field -> Doc
prettyField (BinaryField _)
  = "F2m"
prettyField (ExtensionField)
  = "Fp12"
prettyField (PrimeField _)
  = "Fp"

prettyImport :: Field -> Doc
prettyImport (BinaryField _)
  = "import BinaryField (BinaryField)"
prettyImport (ExtensionField)
  = "import ExtensionField (ExtensionField, IrreducibleMonic(..), fromList, t, x)"
prettyImport (PrimeField _)
  = "import PrimeField (PrimeField)"

prettyType :: Field -> Doc
prettyType (BinaryField p)
  = "type F2m = BinaryField " <> prettyInteger p
prettyType (ExtensionField)
  = "type Fp12 = ExtensionField"
prettyType (PrimeField p)
  = "type Fp = PrimeField " <> prettyInteger p
