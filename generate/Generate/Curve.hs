module Generate.Curve
  ( module Generate.Curve
  ) where

import Protolude

import Text.PrettyPrint.Leijen.Text

import Generate.Pretty
import Generate.Types

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

prettyElement :: Element -> Doc
prettyElement (B n)
  = prettyNatural n
prettyElement (E ns)
  = "toE'" <+> align
    (    (if null ns then "[" else "[ ")
    <>   hcat (punctuate "\n, " (map prettyElement ns))
    <$$> "]"
    )
prettyElement (P n)
  = prettyNatural n

prettyField :: Field -> Doc
prettyField (Binary f2m _)
  = pretty f2m
prettyField (Extension fq' _ _ _ _)
  = pretty fq'
prettyField (Prime fq _)
  = pretty fq

prettyImport :: Doc
prettyImport
  =    "import Protolude"
  <>   prettyBreak
  <$$> "import Data.Field.Galois"
  <$$> "import GHC.Natural (Natural)"

prettyType :: Field -> Doc
prettyType (Binary f2m q)
  = "type" <+> pretty f2m <+> "= Binary" <+> prettyNatural q
prettyType (Extension fq' fq q s k)
  =    prettyType' k
  <$$> "data" <+> pretty q
  <$$> "instance IrreducibleMonic" <+> pretty fq <+> pretty q <+> "where"
  <$$> indent 2
    (   "split _ =" <+> pretty s
    <$$> prettyInline "split"
    )
  <$$> "type" <+> pretty fq' <+> "= Extension" <+> pretty fq <+> pretty q
  where
    prettyType' :: Maybe Field -> Doc
    prettyType' (Just f) = prettyType f
    prettyType' _        = mempty
prettyType (Prime fq q)
  = "type" <+> pretty fq <+> "= Prime" <+> prettyNatural q
