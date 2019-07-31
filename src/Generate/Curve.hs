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
  = "fromList" <+> align
    (    (if null ns then "[" else "[ ")
    <>   hcat (punctuate "\n, " (map prettyElement ns))
    <$$> "]"
    )
prettyElement (PF n)
  = prettyInteger n

prettyField :: Field -> Doc
prettyField (BinaryField f2m _)
  = pretty f2m
prettyField (ExtensionField fq' _ _ _ _)
  = pretty fq'
prettyField (PrimeField fq _)
  = pretty fq

prettyImport :: Field -> Doc
prettyImport k
  =    "import Protolude"
  <>   prettyBreak
  <$$> k'
  <$$> "import PrimeField"
  where
    k' = case k of
      BinaryField _ _          -> "import BinaryField"
      ExtensionField _ _ _ _ _ -> "import ExtensionField"
      _                        -> mempty

prettyType :: Field -> Doc
prettyType (BinaryField f2m q)
  = "type" <+> pretty f2m <+> "= BinaryField" <+> prettyInteger q
prettyType (ExtensionField fq' fq q s k)
  =    prettyType' k
  <$$> "data" <+> pretty q
  <$$> "instance IrreducibleMonic" <+> pretty fq <+> pretty q <+> "where"
  <$$> "  split _ =" <+> pretty s
  <$$> "type" <+> pretty fq' <+> "= ExtensionField" <+> pretty fq <+> pretty q
  where
    prettyType' :: Maybe Field -> Doc
    prettyType' (Just f) = prettyType f
    prettyType' _        = mempty
prettyType (PrimeField fq q)
  = "type" <+> pretty fq <+> "= PrimeField" <+> prettyInteger q
