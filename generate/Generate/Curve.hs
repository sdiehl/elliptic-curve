module Generate.Curve
  ( module Generate.Curve
  ) where

import Protolude

import Text.PrettyPrint.Leijen.Text hiding (char)

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

prettyType :: Field -> Doc -> Doc
prettyType (Binary f2m m) char
  =    "type" <+> pretty f2m <+> "= Binary" <+> char
  <$$> "type" <+> char <+> "=" <+> prettyNatural m
prettyType (Extension fq' fq q s k) char
  =    "type" <+> pretty fq' <+> "= Extension" <+> pretty q <+> pretty fq
  <$$> "data" <+> pretty q
  <$$> "instance IrreducibleMonic" <+> pretty q <+> pretty fq <+> "where"
  <$$> indent 2
    (   "poly _ =" <+> pretty s
    <$$> prettyInline "poly"
    )
  <$$> prettyType' k
  where
    prettyType' :: Maybe Field -> Doc
    prettyType' (Just f) = prettyType f char
    prettyType' _        = mempty
prettyType (Prime fq q) char
  =    "type" <+> pretty fq <+> "= Prime" <+> char
  <$$> "type" <+> char <+> "=" <+> prettyNatural q
