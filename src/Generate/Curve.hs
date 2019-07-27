module Generate.Curve
  ( prettyElement
  , prettyField
  , prettyImport
  , prettyType
  ) where

import Protolude

import Text.PrettyPrint.Leijen.Text

import Generate.Pretty
import Generate.Types

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
prettyField (BinaryField f2m _)
  = pretty f2m
prettyField (ExtensionField fq _ _ _ _)
  = pretty fq
prettyField (PrimeField fp _)
  = pretty fp

prettyImport :: Field -> Doc
prettyImport (BinaryField _ _)
  = "import BinaryField (BinaryField)"
prettyImport (ExtensionField _ _ _ _ _)
  = "import ExtensionField"
prettyImport (PrimeField _ _)
  = "import PrimeField (PrimeField)"

prettyType :: Field -> Doc
prettyType (BinaryField f2m p)
  = "type " <> pretty f2m <> " = BinaryField " <> prettyInteger p
prettyType (ExtensionField fq fp p s k)
  =    prettyType' k
  <$$> "data " <> pretty p
  <$$> "instance IrreducibleMonic " <> pretty fp <> " " <> pretty p <> " where"
  <$$> "  split _ = " <> pretty s
  <$$> "type " <> pretty fq <> " = ExtensionField " <> pretty fp <> " " <> pretty p
  where
    prettyType' :: Maybe Field -> Doc
    prettyType' (Just f) = prettyType f
    prettyType' _        = mempty
prettyType (PrimeField fp p)
  = "type " <> pretty fp <> " = PrimeField " <> prettyInteger p
